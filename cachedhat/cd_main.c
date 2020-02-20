
/*--------------------------------------------------------------------*/
/*--- Cachegrind: everything but the simulation itself.            ---*/
/*---                                                    cd_main.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Cachegrind, a Valgrind tool for cache
   profiling programs.

   Copyright (C) 2002-2017 Nicholas Nethercote
      njn@valgrind.org

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307, USA.

   The GNU General Public License is contained in the file COPYING.
*/

#include "pub_tool_basics.h"
#include "pub_tool_debuginfo.h"
#include "pub_tool_libcbase.h"
#include "pub_tool_libcassert.h"
#include "pub_tool_libcfile.h"
#include "pub_tool_libcprint.h"
#include "pub_tool_libcproc.h"
#include "pub_tool_mallocfree.h"
#include "pub_tool_options.h"
#include "pub_tool_oset.h"
#include "pub_tool_tooliface.h"
#include "pub_tool_xarray.h"
#include "pub_tool_clientstate.h"
#include "pub_tool_machine.h"      // VG_(fnptr_to_fnentry)

#include "pub_tool_replacemalloc.h"
#include "pub_tool_wordfm.h"

#include "cd_arch.h"
#include "cd_sim.c"
#include "cd_branchpred.c"


/*--------------------------------------------------------------------*/
/*--- Malloc profile                                               ---*/
/*--------------------------------------------------------------------*/
//------------------------------------------------------------//
//--- Globals                                              ---//
//------------------------------------------------------------//

#define HISTOGRAM_SIZE_LIMIT 1024
UInt  clo_ts_res = TS_RES;  /* time resolution (#mem reference) */
UInt  clo_mem_res = MEM_RES;/* mem space resolution */
UInt  clo_hm_size_limit = HEATMAP_SIZE_LIMIT;
ULong  clo_hm_writes_limit = HEATMAP_WRITES_LIMIT;
ULong  clo_hm_reads_limit = HEATMAP_READS_LIMIT;

Bool clo_hm_read = False; /* profile read heatmap? */
Bool clo_hm_write = True; /* profile read heatmap? */
// Values for the entire run.
static ULong g_total_blocks = 0;
static ULong g_total_bytes  = 0;

// Current values.
static ULong g_curr_blocks = 0;
static ULong g_curr_bytes  = 0;
static ULong g_curr_instrs = 0;  // incremented from generated code

// Values at the global max, i.e. when g_curr_bytes peaks.
static ULong g_max_blocks = 0;
static ULong g_max_bytes  = 0;
static ULong g_max_instrs = 0;

// Values for the entire run. Computed at the end.
static ULong g_reads_bytes = 0;
static ULong g_writes_bytes = 0;

/* May not contain zero-sized blocks.  May not contain
   overlapping blocks. */
static WordFM* interval_tree = NULL;  /* WordFM* Block* void */

/* Here's the comparison function.  Since the tree is required
to contain non-zero sized, non-overlapping blocks, it's good
enough to consider any overlap as a match. */
static Word interval_tree_Cmp ( UWord k1, UWord k2 )
{
   Block* b1 = (Block*)k1;
   Block* b2 = (Block*)k2;
   tl_assert(b1->req_szB > 0);
   tl_assert(b2->req_szB > 0);
   if (b1->payload + b1->req_szB <= b2->payload) return -1;
   if (b2->payload + b2->req_szB <= b1->payload) return  1;
   return 0;
}

// 2-entry cache for find_Block_containing
static Block* fbc_cache0 = NULL;
static Block* fbc_cache1 = NULL;

static UWord stats__n_fBc_cached = 0;
static UWord stats__n_fBc_uncached = 0;
static UWord stats__n_fBc_notfound = 0;

Block* find_Block_containing ( Addr a )
{
   if (LIKELY(fbc_cache0
              && fbc_cache0->payload <= a
              && a < fbc_cache0->payload + fbc_cache0->req_szB)) {
      // found at 0
      stats__n_fBc_cached++;
      return fbc_cache0;
   }
   if (LIKELY(fbc_cache1
              && fbc_cache1->payload <= a
              && a < fbc_cache1->payload + fbc_cache1->req_szB)) {
      // found at 1; swap 0 and 1
      Block* tmp = fbc_cache0;
      fbc_cache0 = fbc_cache1;
      fbc_cache1 = tmp;
      stats__n_fBc_cached++;
      return fbc_cache0;
   }
   Block fake;
   fake.payload = a;
   fake.req_szB = 1;
   UWord foundkey = 1;
   UWord foundval = 1;
   Bool found = VG_(lookupFM)( interval_tree,
                               &foundkey, &foundval, (UWord)&fake );
   if (!found) {
      stats__n_fBc_notfound++;
      return NULL;
   }
   tl_assert(foundval == 0); // we don't store vals in the interval tree
   tl_assert(foundkey != 1);
   Block* res = (Block*)foundkey;
   tl_assert(res != &fake);
   // put at the top position
   fbc_cache1 = fbc_cache0;
   fbc_cache0 = res;
   stats__n_fBc_uncached++;
   return res;
}

// delete a block; asserts if not found.  (viz, 'a' must be
// known to be present.)
static void delete_Block_starting_at ( Addr a )
{
   Block fake;
   fake.payload = a;
   fake.req_szB = 1;
   Bool found = VG_(delFromFM)( interval_tree,
                                NULL, NULL, (Addr)&fake );
   tl_assert(found);
   fbc_cache0 = fbc_cache1 = NULL;
}


//------------------------------------------------------------//
//--- a FM of allocation points (APs)                      ---//
//------------------------------------------------------------//

typedef
   struct {
      // The allocation point that we're summarising stats for.
      ExeContext* ap;

      // Total number of blocks and bytes allocated by this AP.
      ULong total_blocks;
      ULong total_bytes;

      // The current number of blocks and bytes live for this AP.
      ULong curr_blocks;
      ULong curr_bytes;

      // Values at the AP max, i.e. when this AP's curr_bytes peaks.
      ULong max_blocks;     // Blocks at the AP max.
      ULong max_bytes;      // The AP max, measured in bytes.

      // Values at the global max.
      ULong at_tgmax_blocks;
      ULong at_tgmax_bytes;

      // Total lifetimes of all blocks allocated by this AP. Includes blocks
      // explicitly freed and blocks implicitly freed at termination.
      ULong total_lifetimes_instrs;

      // Number of blocks freed by this AP. (Only used in assertions.)
      ULong freed_blocks;

      // Total number of reads and writes in all blocks allocated
      // by this AP.
      ULong reads_bytes;
      ULong writes_bytes;

      /* Histogram information.  We maintain a histogram aggregated for
         all retiring Blocks allocated by this AP, but only if:
         - this AP has only ever allocated objects of one size
         - that size is <= HISTOGRAM_SIZE_LIMIT
         What we need therefore is a mechanism to see if this AP
         has only ever allocated blocks of one size.

         3 states:
            Unknown          because no retirement yet
            Exactly xsize    all retiring blocks are of this size
            Mixed            multiple different sizes seen
      */
      enum { Unknown=999, Exactly, Mixed } xsize_tag;
      SizeT xsize;
      UInt* histo; /* [0 .. xsize-1] */
      
      HeatMap*       HMHead;
      HeatMap*       HMNode;
   }
   APInfo;

/* maps ExeContext*'s to APInfo*'s.  Note that the keys must match the
   .ap field in the values. */
static WordFM* apinfo = NULL;  /* WordFM* ExeContext* APInfo* */


// Are we at peak memory? If so, update at_tgmax_blocks and at_tgmax_bytes in
// all APInfos. Note that this is moderately expensive so we avoid calling it
// on every allocation.
static void check_for_peak(void)
{
   if (g_curr_bytes == g_max_bytes) {
      // It's a peak. (If there are multiple equal peaks we record the latest
      // one.)
      UWord keyW, valW;
      VG_(initIterFM)(apinfo);
      while (VG_(nextIterFM)(apinfo, &keyW, &valW)) {
         APInfo* api = (APInfo*)valW;
         tl_assert(api && api->ap == (ExeContext*)keyW);
         api->at_tgmax_blocks = api->curr_blocks;
         api->at_tgmax_bytes = api->curr_bytes;
      }
      VG_(doneIterFM)(apinfo);
   }
}


void init_hm_node(HeatMap** head, HeatMap** node, SizeT req_szB) {
   (*head) = VG_(malloc)("dh.new_block.3", sizeof(HeatMap));
   (*head)->mem_region_size = (req_szB/clo_mem_res + 1);
   if (req_szB == 0) {
      (*head)->mem_region_w = NULL; // dummy node
      (*head)->mem_region_r = NULL;
   }
   else {
      if (clo_hm_write) {
         (*head)->mem_region_w = VG_(malloc)("dh.new_block.4", (*head)->mem_region_size * sizeof(UInt));
         VG_(memset)((*head)->mem_region_w, 0, (*head)->mem_region_size * sizeof(UInt));
      }
      else (*head)->mem_region_w = NULL;
      if (clo_hm_read) {
         (*head)->mem_region_r = VG_(malloc)("dh.new_block.5", (*head)->mem_region_size * sizeof(UInt));
         VG_(memset)((*head)->mem_region_r, 0, (*head)->mem_region_size * sizeof(UInt));
      }
      else (*head)->mem_region_r = NULL;
   }
   (*head)->next = NULL;
   (*head)->ts_w = 0;
   (*head)->ts_r = 0;
   *node = (*head);
}

void add_hm_node(HeatMap** node, UInt size) {
   tl_assert((*node) != NULL);
   HeatMap* new_node = VG_(malloc)("dh.new_block.3", sizeof(HeatMap));
   new_node->mem_region_size = size;
   if (clo_hm_write) {
      new_node->mem_region_w = VG_(malloc)("dh.new_block.4", new_node->mem_region_size * sizeof(UInt));
      VG_(memset)(new_node->mem_region_w, 0, new_node->mem_region_size * sizeof(UInt));
   }
   else new_node->mem_region_w = NULL;
   if (clo_hm_read) {
      new_node->mem_region_r = VG_(malloc)("dh.new_block.5", new_node->mem_region_size * sizeof(UInt));
      VG_(memset)(new_node->mem_region_r, 0, new_node->mem_region_size * sizeof(UInt));
   }
   else new_node->mem_region_r = NULL;
   
   
   new_node->next = NULL;
   new_node->ts_w = 0;
   new_node->ts_r = 0;
   (*node)->next = new_node;
   (*node) = new_node;
}

void del_hm_list(HeatMap* head) {
   HeatMap* next = head->next;
   while (next != NULL) {
      next = head->next;
      if (head->mem_region_w != NULL) VG_(free)(head->mem_region_w);
      if (head->mem_region_r != NULL) VG_(free)(head->mem_region_r);
      VG_(free)(head);
      head = next;
   }
}

void move_hm_list(HeatMap** curr, HeatMap* new_head, HeatMap* new_curr) {
   tl_assert((*curr)->next == NULL);
   if (new_head != NULL) {
      (*curr)->next = new_head;
      (*curr) = new_curr;
   }
}


/* 'bk' is being introduced (has just been allocated).  Find the
   relevant APInfo entry for it, or create one, based on the block's
   allocation EC.  Then, update the APInfo to the extent that we
   actually can, to reflect the allocation. */
static void intro_Block ( Block* bk )
{
   tl_assert(bk);
   tl_assert(bk->ap);

   APInfo* api   = NULL;
   UWord   keyW  = 0;
   UWord   valW  = 0;
   Bool    found = VG_(lookupFM)( apinfo,
                                  &keyW, &valW, (UWord)bk->ap );
   if (found) {
      api = (APInfo*)valW;
      tl_assert(keyW == (UWord)bk->ap);
      if (bk->HMHead == NULL && api->curr_bytes >= clo_hm_size_limit) {
         init_hm_node(&(bk->HMHead), &(bk->HMNode), bk->req_szB);
      }
   } else {
      api = VG_(malloc)( "dh.intro_Block.1", sizeof(APInfo) );
      VG_(memset)(api, 0, sizeof(*api));
      api->ap = bk->ap;
      Bool present = VG_(addToFM)( apinfo,
                                   (UWord)bk->ap, (UWord)api );
      tl_assert(!present);
      // histo stuff
      tl_assert(api->freed_blocks == 0);
      api->xsize_tag = Unknown;
      api->xsize = 0;
      if (0) VG_(printf)("api %p   -->  Unknown\n", api);

      // heap heatmap
      api->HMHead = NULL; api->HMNode = NULL;
      init_hm_node(&(api->HMHead), &(api->HMNode), 0);
   }

   tl_assert(api->ap == bk->ap);

   // Update global stats first.

   g_total_blocks++;
   g_total_bytes += bk->req_szB;

   g_curr_blocks++;
   g_curr_bytes += bk->req_szB;
   if (g_curr_bytes > g_max_bytes) {
      g_max_blocks = g_curr_blocks;
      g_max_bytes = g_curr_bytes;
      g_max_instrs = g_curr_instrs;
   }

   // Now update APInfo stats.

   api->total_blocks++;
   api->total_bytes += bk->req_szB;

   api->curr_blocks++;
   api->curr_bytes += bk->req_szB;
   if (api->curr_bytes > api->max_bytes) {
      api->max_blocks = api->curr_blocks;
      api->max_bytes  = api->curr_bytes;
   }
}

/* 'bk' is retiring (being freed).  Find the relevant APInfo entry for
   it, which must already exist.  Then, fold info from 'bk' into that
   entry.  'because_freed' is True if the block is retiring because
   the client has freed it.  If it is False then the block is retiring
   because the program has finished, in which case we want to skip the
   updates of the total blocks live etc for this AP, but still fold in
   the access counts and histo data that have so far accumulated for
   the block. */
static void retire_Block ( Block* bk, Bool because_freed )
{
   tl_assert(bk);
   tl_assert(bk->ap);

   APInfo* api   = NULL;
   UWord   keyW  = 0;
   UWord   valW  = 0;
   Bool    found = VG_(lookupFM)( apinfo,
                                  &keyW, &valW, (UWord)bk->ap );

   tl_assert(found);
   api = (APInfo*)valW;
   tl_assert(api->ap == bk->ap);

   // update stats following this free.
   if (0)
      VG_(printf)("ec %p  api->c_by_l %llu  bk->rszB %llu\n",
                  bk->ap, api->curr_bytes, (ULong)bk->req_szB);

   if (because_freed) {
      // Total bytes is coming down from a possible peak.
      check_for_peak();

      // Then update global stats.
      tl_assert(g_curr_blocks >= 1);
      tl_assert(g_curr_bytes >= bk->req_szB);
      g_curr_blocks--;
      g_curr_bytes -= bk->req_szB;

      // Then update APInfo stats.
      tl_assert(api->curr_blocks >= 1);
      tl_assert(api->curr_bytes >= bk->req_szB);
      api->curr_blocks--;
      api->curr_bytes -= bk->req_szB;

      api->freed_blocks++;
   }

   tl_assert(bk->allocd_at <= g_curr_instrs);
   api->total_lifetimes_instrs += (g_curr_instrs - bk->allocd_at);

   // access counts
   api->reads_bytes += bk->reads_bytes;
   api->writes_bytes += bk->writes_bytes;
   g_reads_bytes += bk->reads_bytes;
   g_writes_bytes += bk->writes_bytes;

   // histo stuff.  First, do state transitions for xsize/xsize_tag.
   switch (api->xsize_tag) {

      case Unknown:
         tl_assert(api->xsize == 0);
         tl_assert(api->freed_blocks == 1 || api->freed_blocks == 0);
         tl_assert(!api->histo);
         api->xsize_tag = Exactly;
         api->xsize = bk->req_szB;
         if (0) VG_(printf)("api %p   -->  Exactly(%lu)\n", api, api->xsize);
         // and allocate the histo
         if (bk->histoW) {
            api->histo = VG_(malloc)("dh.retire_Block.1",
                                     api->xsize * sizeof(UInt));
            VG_(memset)(api->histo, 0, api->xsize * sizeof(UInt));
         }
         break;

      case Exactly:
         //tl_assert(api->freed_blocks > 1);
         if (bk->req_szB != api->xsize) {
            if (0) VG_(printf)("api %p   -->  Mixed(%lu -> %lu)\n",
                               api, api->xsize, bk->req_szB);
            api->xsize_tag = Mixed;
            api->xsize = 0;
            // deallocate the histo, if any
            if (api->histo) {
               VG_(free)(api->histo);
               api->histo = NULL;
            }
         }
         break;

      case Mixed:
         //tl_assert(api->freed_blocks > 1);
         break;

      default:
        tl_assert(0);
   }

   // See if we can fold the histo data from this block into
   // the data for the AP
   if (api->xsize_tag == Exactly && api->histo && bk->histoW) {
      tl_assert(api->xsize == bk->req_szB);
      UWord i;
      for (i = 0; i < api->xsize; i++) {
         // FIXME: do something better in case of overflow of api->histo[..]
         // Right now, at least don't let it overflow/wrap around
         if (api->histo[i] <= 0xFFFE0000)
            api->histo[i] += (UInt)bk->histoW[i];
      }
      if (0) VG_(printf)("fold in, AP = %p\n", api);
   }

   // Heap heatmap list
   if(bk->HMHead != NULL) {
      move_hm_list(&(api->HMNode), bk->HMHead, bk->HMNode);
      //VG_(printf)("api HMHead %p, bk head %p, bk end %p\n", api->HMHead, bk->HMHead, bk->HMNode);
   }

#if 0
   if (bk->histoB) {
      VG_(printf)("block retiring, histo %lu: ", bk->req_szB);
      UWord i;
      for (i = 0; i < bk->req_szB; i++)
        VG_(printf)("%u ", (UInt)bk->histoB[i]);
      VG_(printf)("\n");
   } else {
      VG_(printf)("block retiring, no histo %lu\n", bk->req_szB);
   }
#endif
}

/* This handles block resizing.  When a block with AP 'ec' has a
   size change of 'delta', call here to update the APInfo. */
static void resize_Block(Block *bk, ExeContext* ec, SizeT old_req_szB, SizeT new_req_szB)
{
   Long    delta = (Long)new_req_szB - (Long)old_req_szB;
   APInfo* api   = NULL;
   UWord   keyW  = 0;
   UWord   valW  = 0;
   Bool    found = VG_(lookupFM)( apinfo,
                                  &keyW, &valW, (UWord)ec );

   tl_assert(found);
   api = (APInfo*)valW;
   tl_assert(api->ap == ec);

   if (delta < 0) {
      tl_assert(api->curr_bytes >= -delta);
      tl_assert(g_curr_bytes >= -delta);
   }

   // Total bytes might be coming down from a possible peak.
   if (delta < 0)
      check_for_peak();

   // Note: we treat realloc() like malloc() + free() for total counts, i.e. we
   // increment total_blocks by 1 and increment total_bytes by new_req_szB.
   //
   // A reasonable alternative would be to leave total_blocks unchanged and
   // increment total_bytes by delta (but only if delta is positive). But then
   // calls to realloc wouldn't be counted towards the total_blocks count,
   // which is undesirable.

   // Update global stats first.

   g_total_blocks++;
   g_total_bytes += new_req_szB;

   g_curr_blocks += 0;  // unchanged
   g_curr_bytes += delta;
   if (g_curr_bytes > g_max_bytes) {
      g_max_blocks = g_curr_blocks;
      g_max_bytes = g_curr_bytes;
      g_max_instrs = g_curr_instrs;
   }

   // Now update APInfo stats.

   api->total_blocks++;
   api->total_bytes += new_req_szB;

   api->curr_blocks += 0;  // unchanged
   api->curr_bytes += delta;
   if (api->curr_bytes > api->max_bytes) {
      api->max_blocks = api->curr_blocks;
      api->max_bytes  = api->curr_bytes;
   }

   // update heatmap node info
   if (bk->HMNode != NULL) {
      add_hm_node(&(bk->HMNode), (new_req_szB/clo_mem_res + 1)); // add node with new req size
   }
   else { // in case realloc is called before HeatMap node is allocated
      if (new_req_szB >= clo_hm_size_limit || api->curr_bytes >= clo_hm_size_limit)
         init_hm_node(&(bk->HMHead), &(bk->HMNode), new_req_szB);
   }
}


//------------------------------------------------------------//
//--- update both Block and APInfos after {m,re}alloc/free ---//
//------------------------------------------------------------//

static
void* new_block ( ThreadId tid, void* p, SizeT req_szB, SizeT req_alignB,
                  Bool is_zeroed )
{
   tl_assert(p == NULL); // don't handle custom allocators right now
   SizeT actual_szB /*, slop_szB*/;
   if ((SSizeT)req_szB < 0) return NULL;

   if (req_szB == 0)
      req_szB = 1;  /* can't allow zero-sized blocks in the interval tree */

   // Allocate and zero if necessary
   if (!p) {
      p = VG_(cli_malloc)( req_alignB, req_szB );
      if (!p) {
         return NULL;
      }
      if (is_zeroed) VG_(memset)(p, 0, req_szB);
      actual_szB = VG_(cli_malloc_usable_size)(p);
      tl_assert(actual_szB >= req_szB);
      /* slop_szB = actual_szB - req_szB; */
   } else {
      /* slop_szB = 0; */
   }

   // Make new Block, add to interval_tree.
   Block* bk = VG_(malloc)("dh.new_block.1", sizeof(Block));
   bk->payload      = (Addr)p;
   bk->req_szB      = req_szB;
   bk->ap           = VG_(record_ExeContext)(tid, 0/*first word delta*/);
   bk->allocd_at    = g_curr_instrs;
   bk->reads_bytes  = 0;
   bk->writes_bytes = 0;
   // set up histogram array, if the block isn't too large
   bk->histoW = NULL;
   if (req_szB <= HISTOGRAM_SIZE_LIMIT) {
      bk->histoW = VG_(malloc)("dh.new_block.2", req_szB * sizeof(UShort));
      VG_(memset)(bk->histoW, 0, req_szB * sizeof(UShort));
   }

   // set up heap heatmap list
   bk->HMHead = NULL; bk->HMNode = NULL;
   if (req_szB >= clo_hm_size_limit) {
      init_hm_node(&(bk->HMHead), &(bk->HMNode), req_szB);
   }

   Bool present = VG_(addToFM)( interval_tree, (UWord)bk, (UWord)0/*no val*/);
   tl_assert(!present);
   fbc_cache0 = fbc_cache1 = NULL;

   intro_Block(bk);

   if (0) VG_(printf)("ALLOC %lu -> %p\n", req_szB, p);

   return p;
}

static
void die_block ( void* p, Bool custom_free )
{
   tl_assert(!custom_free);  // at least for now

   Block* bk = find_Block_containing( (Addr)p );

   if (!bk) {
     return; // bogus free
   }

   tl_assert(bk->req_szB > 0);
   // assert the block finder is behaving sanely
   tl_assert(bk->payload <= (Addr)p);
   tl_assert( (Addr)p < bk->payload + bk->req_szB );

   if (bk->payload != (Addr)p) {
      return; // bogus free
   }

   if (0) VG_(printf)(" FREE %p %llu\n",
                      p, g_curr_instrs - bk->allocd_at);

   retire_Block(bk, True/*because_freed*/);

   VG_(cli_free)( (void*)bk->payload );
   delete_Block_starting_at( bk->payload );
   if (bk->histoW) {
      VG_(free)( bk->histoW );
      bk->histoW = NULL;
   }
   VG_(free)( bk );
}


static
void* renew_block ( ThreadId tid, void* p_old, SizeT new_req_szB )
{
   if (0) VG_(printf)("REALL %p %lu\n", p_old, new_req_szB);
   void* p_new = NULL;

   tl_assert(new_req_szB > 0); // map 0 to 1

   // Find the old block.
   Block* bk = find_Block_containing( (Addr)p_old );
   if (!bk) {
      return NULL;   // bogus realloc
   }

   tl_assert(bk->req_szB > 0);
   // assert the block finder is behaving sanely
   tl_assert(bk->payload <= (Addr)p_old);
   tl_assert( (Addr)p_old < bk->payload + bk->req_szB );

   if (bk->payload != (Addr)p_old) {
      return NULL; // bogus realloc
   }

   // Keeping the histogram alive in any meaningful way across
   // block resizing is too darn complicated.  Just throw it away.
   if (bk->histoW) {
      VG_(free)(bk->histoW);
      bk->histoW = NULL;
   }

   // Actually do the allocation, if necessary.
   if (new_req_szB <= bk->req_szB) {

      // New size is smaller or same; block not moved.
      resize_Block(bk, bk->ap, bk->req_szB, new_req_szB);
      bk->req_szB = new_req_szB;
      return p_old;

   } else {

      // New size is bigger;  make new block, copy shared contents, free old.
      p_new = VG_(cli_malloc)(VG_(clo_alignment), new_req_szB);
      if (!p_new) {
         // Nb: if realloc fails, NULL is returned but the old block is not
         // touched.  What an awful function.
         return NULL;
      }
      tl_assert(p_new != p_old);

      VG_(memcpy)(p_new, p_old, bk->req_szB);
      VG_(cli_free)(p_old);

      // Since the block has moved, we need to re-insert it into the
      // interval tree at the new place.  Do this by removing
      // and re-adding it.
      delete_Block_starting_at( (Addr)p_old );
      // now 'bk' is no longer in the tree, but the Block itself
      // is still alive

      // Update the metadata.
      resize_Block(bk, bk->ap, bk->req_szB, new_req_szB);
      bk->payload = (Addr)p_new;
      bk->req_szB = new_req_szB;

      // and re-add
      Bool present
         = VG_(addToFM)( interval_tree, (UWord)bk, (UWord)0/*no val*/);
      tl_assert(!present);
      fbc_cache0 = fbc_cache1 = NULL;

      return p_new;
   }
   /*NOTREACHED*/
   tl_assert(0);
}


//------------------------------------------------------------//
//--- malloc() et al replacement wrappers                  ---//
//------------------------------------------------------------//

static void* dh_malloc ( ThreadId tid, SizeT szB )
{
   return new_block( tid, NULL, szB, VG_(clo_alignment), /*is_zeroed*/False );
}

static void* dh___builtin_new ( ThreadId tid, SizeT szB )
{
   return new_block( tid, NULL, szB, VG_(clo_alignment), /*is_zeroed*/False );
}

static void* dh___builtin_vec_new ( ThreadId tid, SizeT szB )
{
   return new_block( tid, NULL, szB, VG_(clo_alignment), /*is_zeroed*/False );
}

static void* dh_calloc ( ThreadId tid, SizeT m, SizeT szB )
{
   return new_block( tid, NULL, m*szB, VG_(clo_alignment), /*is_zeroed*/True );
}

static void *dh_memalign ( ThreadId tid, SizeT alignB, SizeT szB )
{
   return new_block( tid, NULL, szB, alignB, False );
}

static void dh_free ( ThreadId tid __attribute__((unused)), void* p )
{
   die_block( p, /*custom_free*/False );
}

static void dh___builtin_delete ( ThreadId tid, void* p )
{
   die_block( p, /*custom_free*/False);
}

static void dh___builtin_vec_delete ( ThreadId tid, void* p )
{
   die_block( p, /*custom_free*/False );
}

static void* dh_realloc ( ThreadId tid, void* p_old, SizeT new_szB )
{
   if (p_old == NULL) {
      return dh_malloc(tid, new_szB);
   }
   if (new_szB == 0) {
      dh_free(tid, p_old);
      return NULL;
   }
   return renew_block(tid, p_old, new_szB);
}

static SizeT dh_malloc_usable_size ( ThreadId tid, void* p )
{
   Block* bk = find_Block_containing( (Addr)p );
   return bk ? bk->req_szB : 0;
}


//------------------------------------------------------------//
//--- memory references                                    ---//
//------------------------------------------------------------//

static
void inc_histo_for_block ( Block* bk, Addr addr, UWord szB )
{
   UWord i, offMin, offMax1;
   offMin = addr - bk->payload;
   tl_assert(offMin < bk->req_szB);
   offMax1 = offMin + szB;
   if (offMax1 > bk->req_szB)
      offMax1 = bk->req_szB;
   //VG_(printf)("%lu %lu   (size of block %lu)\n", offMin, offMax1, bk->req_szB);
   for (i = offMin; i < offMax1; i++) {
      UShort n = bk->histoW[i];
      if (n < 0xFFFF) n++;
      bk->histoW[i] = n;
   }
}

/*------------------------------------------------------------*/
/*--- Constants                                            ---*/
/*------------------------------------------------------------*/

/* Set to 1 for very verbose debugging */
#define DEBUG_CG 0

/*------------------------------------------------------------*/
/*--- Options                                              ---*/
/*------------------------------------------------------------*/

static Bool  clo_cache_sim  = True;  /* do cache simulation? */
static Bool  clo_branch_sim = False; /* do branch simulation? */

static const HChar* clo_cachegrind_out_file = "cachegrind.out.%p";

/*------------------------------------------------------------*/
/*--- Cachesim configuration                               ---*/
/*------------------------------------------------------------*/

static Int min_line_size = 0; /* min of L1 and LL cache line sizes */

/*------------------------------------------------------------*/
/*--- Types and Data Structures                            ---*/
/*------------------------------------------------------------*/

typedef
   struct {
      ULong a;  /* total # memory accesses of this kind */
      ULong m1; /* misses in the first level cache */
      ULong mL; /* misses in the second level cache */
   }
   CacheCC;

typedef
   struct {
      ULong b;  /* total # branches of this kind */
      ULong mp; /* number of branches mispredicted */
   }
   BranchCC;

//------------------------------------------------------------
// Primary data structure #1: CC table
// - Holds the per-source-line hit/miss stats, grouped by file/function/line.
// - an ordered set of CCs.  CC indexing done by file/function/line (as
//   determined from the instrAddr).
// - Traversed for dumping stats at end in file/func/line hierarchy.

typedef struct {
   HChar* file;
   const HChar* fn;
   Int    line;
}
CodeLoc;

typedef struct {
   CodeLoc  loc; /* Source location that these counts pertain to */
   CacheCC  Ir;  /* Insn read counts */
   CacheCC  Dr;  /* Data read counts */
   CacheCC  Dw;  /* Data write/modify counts */
   BranchCC Bc;  /* Conditional branch counts */
   BranchCC Bi;  /* Indirect branch counts */
} LineCC;

// First compare file, then fn, then line.
static Word cmp_CodeLoc_LineCC(const void *vloc, const void *vcc)
{
   Word res;
   const CodeLoc* a = (const CodeLoc*)vloc;
   const CodeLoc* b = &(((const LineCC*)vcc)->loc);

   res = VG_(strcmp)(a->file, b->file);
   if (0 != res)
      return res;

   res = VG_(strcmp)(a->fn, b->fn);
   if (0 != res)
      return res;

   return a->line - b->line;
}

static OSet* CC_table;

//------------------------------------------------------------
// Primary data structure #2: InstrInfo table
// - Holds the cached info about each instr that is used for simulation.
// - table(SB_start_addr, list(InstrInfo))
// - For each SB, each InstrInfo in the list holds info about the
//   instruction (instrLen, instrAddr, etc), plus a pointer to its line
//   CC.  This node is what's passed to the simulation function.
// - When SBs are discarded the relevant list(instr_details) is freed.

typedef struct _InstrInfo InstrInfo;
struct _InstrInfo {
   Addr    instr_addr;
   UChar   instr_len;
   LineCC* parent;         // parent line-CC
};

typedef struct _SB_info SB_info;
struct _SB_info {
   Addr      SB_addr;      // key;  MUST BE FIRST
   Int       n_instrs;
   InstrInfo instrs[0];
};

static OSet* instrInfoTable;

//------------------------------------------------------------
// Secondary data structure: string table
// - holds strings, avoiding dups
// - used for filenames and function names, each of which will be
//   pointed to by one or more CCs.
// - it also allows equality checks just by pointer comparison, which
//   is good when printing the output file at the end.

static OSet* stringTable;

//------------------------------------------------------------
// Stats
static Int  distinct_files      = 0;
static Int  distinct_fns        = 0;
static Int  distinct_lines      = 0;
static Int  distinct_instrsGen  = 0;
static Int  distinct_instrsNoX  = 0;

static Int  full_debugs         = 0;
static Int  file_line_debugs    = 0;
static Int  fn_debugs           = 0;
static Int  no_debugs           = 0;

/*------------------------------------------------------------*/
/*--- String table operations                              ---*/
/*------------------------------------------------------------*/

static Word stringCmp( const void* key, const void* elem )
{
   return VG_(strcmp)(*(const HChar *const *)key, *(const HChar *const *)elem);
}

// Get a permanent string;  either pull it out of the string table if it's
// been encountered before, or dup it and put it into the string table.
static HChar* get_perm_string(const HChar* s)
{
   HChar** s_ptr = VG_(OSetGen_Lookup)(stringTable, &s);
   if (s_ptr) {
      return *s_ptr;
   } else {
      HChar** s_node = VG_(OSetGen_AllocNode)(stringTable, sizeof(HChar*));
      *s_node = VG_(strdup)("cd.main.gps.1", s);
      VG_(OSetGen_Insert)(stringTable, s_node);
      return *s_node;
   }
}

/*------------------------------------------------------------*/
/*--- CC table operations                                  ---*/
/*------------------------------------------------------------*/

static void get_debug_info(Addr instr_addr, const HChar **dir,
                           const HChar **file, const HChar **fn, UInt* line)
{
   DiEpoch ep = VG_(current_DiEpoch)();
   Bool found_file_line = VG_(get_filename_linenum)(
                             ep,
                             instr_addr, 
                             file, dir,
                             line
                          );
   Bool found_fn        = VG_(get_fnname)(ep, instr_addr, fn);

   if (!found_file_line) {
      *file = "???";
      *line = 0;
   }
   if (!found_fn) {
      *fn = "???";
   }

   if (found_file_line) {
      if (found_fn) full_debugs++;
      else          file_line_debugs++;
   } else {
      if (found_fn) fn_debugs++;
      else          no_debugs++;
   }
}

// Do a three step traversal: by file, then fn, then line.
// Returns a pointer to the line CC, creates a new one if necessary.
static LineCC* get_lineCC(Addr origAddr)
{
   const HChar *fn, *file, *dir;
   UInt    line;
   CodeLoc loc;
   LineCC* lineCC;

   get_debug_info(origAddr, &dir, &file, &fn, &line);

   // Form an absolute pathname if a directory is available
   HChar absfile[VG_(strlen)(dir) + 1 + VG_(strlen)(file) + 1];

   if (dir[0]) {
      VG_(sprintf)(absfile, "%s/%s", dir, file);
   } else {
      VG_(sprintf)(absfile, "%s", file);
   }

   loc.file = absfile;
   loc.fn   = fn;
   loc.line = line;

   lineCC = VG_(OSetGen_Lookup)(CC_table, &loc);
   if (!lineCC) {
      // Allocate and zero a new node.
      lineCC           = VG_(OSetGen_AllocNode)(CC_table, sizeof(LineCC));
      lineCC->loc.file = get_perm_string(loc.file);
      lineCC->loc.fn   = get_perm_string(loc.fn);
      lineCC->loc.line = loc.line;
      lineCC->Ir.a     = 0;
      lineCC->Ir.m1    = 0;
      lineCC->Ir.mL    = 0;
      lineCC->Dr.a     = 0;
      lineCC->Dr.m1    = 0;
      lineCC->Dr.mL    = 0;
      lineCC->Dw.a     = 0;
      lineCC->Dw.m1    = 0;
      lineCC->Dw.mL    = 0;
      lineCC->Bc.b     = 0;
      lineCC->Bc.mp    = 0;
      lineCC->Bi.b     = 0;
      lineCC->Bi.mp    = 0;
      VG_(OSetGen_Insert)(CC_table, lineCC);
   }

   return lineCC;
}

/*------------------------------------------------------------*/
/*--- Cache simulation functions                           ---*/
/*------------------------------------------------------------*/

/* A common case for an instruction read event is that the
 * bytes read belong to the same cache line in both L1I and LL
 * (if cache line sizes of L1 and LL are the same).
 * As this can be detected at instrumentation time, and results
 * in faster simulation, special-casing is benefical.
 *
 * Abbreviations used in var/function names:
 *  IrNoX - instruction read does not cross cache lines
 *  IrGen - generic instruction read; not detected as IrNoX
 *  Ir    - not known / not important whether it is an IrNoX
 */

// Only used with --cache-sim=no.
static VG_REGPARM(1)
void log_1Ir(InstrInfo* n)
{
   n->parent->Ir.a++;
}

// Only used with --cache-sim=no.
static VG_REGPARM(2)
void log_2Ir(InstrInfo* n, InstrInfo* n2)
{
   n->parent->Ir.a++;
   n2->parent->Ir.a++;
}

// Only used with --cache-sim=no.
static VG_REGPARM(3)
void log_3Ir(InstrInfo* n, InstrInfo* n2, InstrInfo* n3)
{
   n->parent->Ir.a++;
   n2->parent->Ir.a++;
   n3->parent->Ir.a++;
}

// Generic case for instruction reads: may cross cache lines.
// All other Ir handlers expect IrNoX instruction reads.
static VG_REGPARM(1)
void log_1IrGen_0D_cache_access(InstrInfo* n)
{
   //VG_(printf)("1IrGen_0D :  CCaddr=0x%010lx,  iaddr=0x%010lx,  isize=%lu\n",
   //             n, n->instr_addr, n->instr_len);
   cachesim_I1_doref_Gen(n->instr_addr, n->instr_len,
			 &n->parent->Ir.m1, &n->parent->Ir.mL);
   n->parent->Ir.a++;
}

static VG_REGPARM(1)
void log_1IrNoX_0D_cache_access(InstrInfo* n)
{
   //VG_(printf)("1IrNoX_0D :  CCaddr=0x%010lx,  iaddr=0x%010lx,  isize=%lu\n",
   //             n, n->instr_addr, n->instr_len);
   cachesim_I1_doref_NoX(n->instr_addr, n->instr_len,
			 &n->parent->Ir.m1, &n->parent->Ir.mL);
   n->parent->Ir.a++;
}

static VG_REGPARM(2)
void log_2IrNoX_0D_cache_access(InstrInfo* n, InstrInfo* n2)
{
   //VG_(printf)("2IrNoX_0D : CC1addr=0x%010lx, i1addr=0x%010lx, i1size=%lu\n"
   //            "            CC2addr=0x%010lx, i2addr=0x%010lx, i2size=%lu\n",
   //            n,  n->instr_addr,  n->instr_len,
   //            n2, n2->instr_addr, n2->instr_len);
   cachesim_I1_doref_NoX(n->instr_addr, n->instr_len,
			 &n->parent->Ir.m1, &n->parent->Ir.mL);
   n->parent->Ir.a++;
   cachesim_I1_doref_NoX(n2->instr_addr, n2->instr_len,
			 &n2->parent->Ir.m1, &n2->parent->Ir.mL);
   n2->parent->Ir.a++;
}

static VG_REGPARM(3)
void log_3IrNoX_0D_cache_access(InstrInfo* n, InstrInfo* n2, InstrInfo* n3)
{
   //VG_(printf)("3IrNoX_0D : CC1addr=0x%010lx, i1addr=0x%010lx, i1size=%lu\n"
   //            "            CC2addr=0x%010lx, i2addr=0x%010lx, i2size=%lu\n"
   //            "            CC3addr=0x%010lx, i3addr=0x%010lx, i3size=%lu\n",
   //            n,  n->instr_addr,  n->instr_len,
   //            n2, n2->instr_addr, n2->instr_len,
   //            n3, n3->instr_addr, n3->instr_len);
   cachesim_I1_doref_NoX(n->instr_addr, n->instr_len,
			 &n->parent->Ir.m1, &n->parent->Ir.mL);
   n->parent->Ir.a++;
   cachesim_I1_doref_NoX(n2->instr_addr, n2->instr_len,
			 &n2->parent->Ir.m1, &n2->parent->Ir.mL);
   n2->parent->Ir.a++;
   cachesim_I1_doref_NoX(n3->instr_addr, n3->instr_len,
			 &n3->parent->Ir.m1, &n3->parent->Ir.mL);
   n3->parent->Ir.a++;
}

static VG_REGPARM(3)
void log_1IrNoX_1Dr_cache_access(InstrInfo* n, Addr data_addr, Word data_size)
{
   //VG_(printf)("1IrNoX_1Dr:  CCaddr=0x%010lx,  iaddr=0x%010lx,  isize=%lu\n"
   //            "                               daddr=0x%010lx,  dsize=%lu\n",
   //            n, n->instr_addr, n->instr_len, data_addr, data_size);
   cachesim_I1_doref_NoX(n->instr_addr, n->instr_len,
			 &n->parent->Ir.m1, &n->parent->Ir.mL);
   n->parent->Ir.a++;

   cachesim_D1_doref(data_addr, data_size, 
                     &n->parent->Dr.m1, &n->parent->Dr.mL, 0);
   n->parent->Dr.a++;
}

static VG_REGPARM(3)
void log_1IrNoX_1Dw_cache_access(InstrInfo* n, Addr data_addr, Word data_size)
{
   //VG_(printf)("1IrNoX_1Dw:  CCaddr=0x%010lx,  iaddr=0x%010lx,  isize=%lu\n"
   //            "                               daddr=0x%010lx,  dsize=%lu\n",
   //            n, n->instr_addr, n->instr_len, data_addr, data_size);
   cachesim_I1_doref_NoX(n->instr_addr, n->instr_len,
			 &n->parent->Ir.m1, &n->parent->Ir.mL);
   n->parent->Ir.a++;

   cachesim_D1_doref(data_addr, data_size, 
                     &n->parent->Dw.m1, &n->parent->Dw.mL, 1);
   n->parent->Dw.a++;
}

/* Note that addEvent_D_guarded assumes that log_0Ir_1Dr_cache_access
   and log_0Ir_1Dw_cache_access have exactly the same prototype.  If
   you change them, you must change addEvent_D_guarded too. */
static VG_REGPARM(3)
void log_0Ir_1Dr_cache_access(InstrInfo* n, Addr data_addr, Word data_size)
{
   //VG_(printf)("0Ir_1Dr:  CCaddr=0x%010lx,  daddr=0x%010lx,  dsize=%lu\n",
   //            n, data_addr, data_size);
   cachesim_D1_doref(data_addr, data_size, 
                     &n->parent->Dr.m1, &n->parent->Dr.mL, 0);
   n->parent->Dr.a++;
}

/* See comment on log_0Ir_1Dr_cache_access. */
static VG_REGPARM(3)
void log_0Ir_1Dw_cache_access(InstrInfo* n, Addr data_addr, Word data_size)
{
   //VG_(printf)("0Ir_1Dw:  CCaddr=0x%010lx,  daddr=0x%010lx,  dsize=%lu\n",
   //            n, data_addr, data_size);
   cachesim_D1_doref(data_addr, data_size, 
                     &n->parent->Dw.m1, &n->parent->Dw.mL, 1);
   n->parent->Dw.a++;
}

/* For branches, we consult two different predictors, one which
   predicts taken/untaken for conditional branches, and the other
   which predicts the branch target address for indirect branches
   (jump-to-register style ones). */

static VG_REGPARM(2)
void log_cond_branch(InstrInfo* n, Word taken)
{
   //VG_(printf)("cbrnch:  CCaddr=0x%010lx,  taken=0x%010lx\n",
   //             n, taken);
   n->parent->Bc.b++;
   n->parent->Bc.mp 
      += (1 & do_cond_branch_predict(n->instr_addr, taken));
}

static VG_REGPARM(2)
void log_ind_branch(InstrInfo* n, UWord actual_dst)
{
   //VG_(printf)("ibrnch:  CCaddr=0x%010lx,    dst=0x%010lx\n",
   //             n, actual_dst);
   n->parent->Bi.b++;
   n->parent->Bi.mp
      += (1 & do_ind_branch_predict(n->instr_addr, actual_dst));
}


/*------------------------------------------------------------*/
/*--- Instrumentation types and structures                 ---*/
/*------------------------------------------------------------*/

/* Maintain an ordered list of memory events which are outstanding, in
   the sense that no IR has yet been generated to do the relevant
   helper calls.  The BB is scanned top to bottom and memory events
   are added to the end of the list, merging with the most recent
   notified event where possible (Dw immediately following Dr and
   having the same size and EA can be merged).

   This merging is done so that for architectures which have
   load-op-store instructions (x86, amd64), the insn is treated as if
   it makes just one memory reference (a modify), rather than two (a
   read followed by a write at the same address).

   At various points the list will need to be flushed, that is, IR
   generated from it.  That must happen before any possible exit from
   the block (the end, or an IRStmt_Exit).  Flushing also takes place
   when there is no space to add a new event.

   If we require the simulation statistics to be up to date with
   respect to possible memory exceptions, then the list would have to
   be flushed before each memory reference.  That would however lose
   performance by inhibiting event-merging during flushing.

   Flushing the list consists of walking it start to end and emitting
   instrumentation IR for each event, in the order in which they
   appear.  It may be possible to emit a single call for two adjacent
   events in order to reduce the number of helper function calls made.
   For example, it could well be profitable to handle two adjacent Ir
   events with a single helper call.  */

typedef
   IRExpr 
   IRAtom;

typedef 
   enum { 
      Ev_IrNoX,  // Instruction read not crossing cache lines
      Ev_IrGen,  // Generic Ir, not being detected as IrNoX
      Ev_Dr,     // Data read
      Ev_Dw,     // Data write
      Ev_Dm,     // Data modify (read then write)
      Ev_Bc,     // branch conditional
      Ev_Bi      // branch indirect (to unknown destination)
   }
   EventTag;

typedef
   struct {
      EventTag   tag;
      InstrInfo* inode;
      union {
         struct {
         } IrGen;
         struct {
         } IrNoX;
         struct {
            IRAtom* ea;
            Int     szB;
         } Dr;
         struct {
            IRAtom* ea;
            Int     szB;
         } Dw;
         struct {
            IRAtom* ea;
            Int     szB;
         } Dm;
         struct {
            IRAtom* taken; /* :: Ity_I1 */
         } Bc;
         struct {
            IRAtom* dst;
         } Bi;
      } Ev;
   }
   Event;

static void init_Event ( Event* ev ) {
   VG_(memset)(ev, 0, sizeof(Event));
}

static IRAtom* get_Event_dea ( Event* ev ) {
   switch (ev->tag) {
      case Ev_Dr: return ev->Ev.Dr.ea;
      case Ev_Dw: return ev->Ev.Dw.ea;
      case Ev_Dm: return ev->Ev.Dm.ea;
      default:    tl_assert(0);
   }
}

static Int get_Event_dszB ( Event* ev ) {
   switch (ev->tag) {
      case Ev_Dr: return ev->Ev.Dr.szB;
      case Ev_Dw: return ev->Ev.Dw.szB;
      case Ev_Dm: return ev->Ev.Dm.szB;
      default:    tl_assert(0);
   }
}


/* Up to this many unnotified events are allowed.  Number is
   arbitrary.  Larger numbers allow more event merging to occur, but
   potentially induce more spilling due to extending live ranges of
   address temporaries. */
#define N_EVENTS 16


/* A struct which holds all the running state during instrumentation.
   Mostly to avoid passing loads of parameters everywhere. */
typedef
   struct {
      /* The current outstanding-memory-event list. */
      Event events[N_EVENTS];
      Int   events_used;

      /* The array of InstrInfo bins for the BB. */
      SB_info* sbInfo;

      /* Number InstrInfo bins 'used' so far. */
      Int sbInfo_i;

      /* The output SB being constructed. */
      IRSB* sbOut;
   }
   CdState;


/*------------------------------------------------------------*/
/*--- Instrumentation main                                 ---*/
/*------------------------------------------------------------*/

// Note that origAddr is the real origAddr, not the address of the first
// instruction in the block (they can be different due to redirection).
static
SB_info* get_SB_info(IRSB* sbIn, Addr origAddr)
{
   Int      i, n_instrs;
   IRStmt*  st;
   SB_info* sbInfo;

   // Count number of original instrs in SB
   n_instrs = 0;
   for (i = 0; i < sbIn->stmts_used; i++) {
      st = sbIn->stmts[i];
      if (Ist_IMark == st->tag) n_instrs++;
   }

   // Check that we don't have an entry for this BB in the instr-info table.
   // If this assertion fails, there has been some screwup:  some
   // translations must have been discarded but Cachegrind hasn't discarded
   // the corresponding entries in the instr-info table.
   sbInfo = VG_(OSetGen_Lookup)(instrInfoTable, &origAddr);
   tl_assert(NULL == sbInfo);

   // BB never translated before (at this address, at least;  could have
   // been unloaded and then reloaded elsewhere in memory)
   sbInfo = VG_(OSetGen_AllocNode)(instrInfoTable,
                                sizeof(SB_info) + n_instrs*sizeof(InstrInfo)); 
   sbInfo->SB_addr  = origAddr;
   sbInfo->n_instrs = n_instrs;
   VG_(OSetGen_Insert)( instrInfoTable, sbInfo );

   return sbInfo;
}


static void showEvent ( Event* ev )
{
   switch (ev->tag) {
      case Ev_IrGen:
         VG_(printf)("IrGen %p\n", ev->inode);
         break;
      case Ev_IrNoX:
         VG_(printf)("IrNoX %p\n", ev->inode);
         break;
      case Ev_Dr:
         VG_(printf)("Dr %p %d EA=", ev->inode, ev->Ev.Dr.szB);
         ppIRExpr(ev->Ev.Dr.ea); 
         VG_(printf)("\n");
         break;
      case Ev_Dw:
         VG_(printf)("Dw %p %d EA=", ev->inode, ev->Ev.Dw.szB);
         ppIRExpr(ev->Ev.Dw.ea); 
         VG_(printf)("\n");
         break;
      case Ev_Dm:
         VG_(printf)("Dm %p %d EA=", ev->inode, ev->Ev.Dm.szB);
         ppIRExpr(ev->Ev.Dm.ea); 
         VG_(printf)("\n");
         break;
      case Ev_Bc:
         VG_(printf)("Bc %p   GA=", ev->inode);
         ppIRExpr(ev->Ev.Bc.taken); 
         VG_(printf)("\n");
         break;
      case Ev_Bi:
         VG_(printf)("Bi %p  DST=", ev->inode);
         ppIRExpr(ev->Ev.Bi.dst); 
         VG_(printf)("\n");
         break;
      default: 
         tl_assert(0);
         break;
   }
}

// Reserve and initialise an InstrInfo for the first mention of a new insn.
static
InstrInfo* setup_InstrInfo ( CdState* gds, Addr instr_addr, UInt instr_len )
{
   InstrInfo* i_node;
   tl_assert(gds->sbInfo_i >= 0);
   tl_assert(gds->sbInfo_i < gds->sbInfo->n_instrs);
   i_node = &gds->sbInfo->instrs[ gds->sbInfo_i ];
   i_node->instr_addr = instr_addr;
   i_node->instr_len  = instr_len;
   i_node->parent     = get_lineCC(instr_addr);
   gds->sbInfo_i++;
   return i_node;
}


/* Generate code for all outstanding memory events, and mark the queue
   empty.  Code is generated into gds->bbOut, and this activity
   'consumes' slots in gds->sbInfo. */

static void flushEvents ( CdState* gds )
{
   Int        i, regparms;
   const HChar* helperName;
   void*      helperAddr;
   IRExpr**   argv;
   IRExpr*    i_node_expr;
   IRDirty*   di;
   Event*     ev;
   Event*     ev2;
   Event*     ev3;

   i = 0;
   while (i < gds->events_used) {

      helperName = NULL;
      helperAddr = NULL;
      argv       = NULL;
      regparms   = 0;

      /* generate IR to notify event i and possibly the ones
         immediately following it. */
      tl_assert(i >= 0 && i < gds->events_used);

      ev  = &gds->events[i];
      ev2 = ( i < gds->events_used-1 ? &gds->events[i+1] : NULL );
      ev3 = ( i < gds->events_used-2 ? &gds->events[i+2] : NULL );
      
      if (DEBUG_CG) {
         VG_(printf)("   flush "); 
         showEvent( ev );
      }

      i_node_expr = mkIRExpr_HWord( (HWord)ev->inode );

      /* Decide on helper fn to call and args to pass it, and advance
         i appropriately. */
      switch (ev->tag) {
         case Ev_IrNoX:
            /* Merge an IrNoX with a following Dr/Dm. */
            if (ev2 && (ev2->tag == Ev_Dr || ev2->tag == Ev_Dm)) {
               /* Why is this true?  It's because we're merging an Ir
                  with a following Dr or Dm.  The Ir derives from the
                  instruction's IMark and the Dr/Dm from data
                  references which follow it.  In short it holds
                  because each insn starts with an IMark, hence an
                  Ev_Ir, and so these Dr/Dm must pertain to the
                  immediately preceding Ir.  Same applies to analogous
                  assertions in the subsequent cases. */
               tl_assert(ev2->inode == ev->inode);
               helperName = "log_1IrNoX_1Dr_cache_access";
               helperAddr = &log_1IrNoX_1Dr_cache_access;
               argv = mkIRExprVec_3( i_node_expr,
                                     get_Event_dea(ev2),
                                     mkIRExpr_HWord( get_Event_dszB(ev2) ) );
               regparms = 3;
               i += 2;
            }
            /* Merge an IrNoX with a following Dw. */
            else
            if (ev2 && ev2->tag == Ev_Dw) {
               tl_assert(ev2->inode == ev->inode);
               helperName = "log_1IrNoX_1Dw_cache_access";
               helperAddr = &log_1IrNoX_1Dw_cache_access;
               argv = mkIRExprVec_3( i_node_expr,
                                     get_Event_dea(ev2),
                                     mkIRExpr_HWord( get_Event_dszB(ev2) ) );
               regparms = 3;
               i += 2;
            }
            /* Merge an IrNoX with two following IrNoX's. */
            else
            if (ev2 && ev3 && ev2->tag == Ev_IrNoX && ev3->tag == Ev_IrNoX)
            {
               if (clo_cache_sim) {
                  helperName = "log_3IrNoX_0D_cache_access";
                  helperAddr = &log_3IrNoX_0D_cache_access;
               } else {
                  helperName = "log_3Ir";
                  helperAddr = &log_3Ir;
               }
               argv = mkIRExprVec_3( i_node_expr, 
                                     mkIRExpr_HWord( (HWord)ev2->inode ), 
                                     mkIRExpr_HWord( (HWord)ev3->inode ) );
               regparms = 3;
               i += 3;
            }
            /* Merge an IrNoX with one following IrNoX. */
            else
            if (ev2 && ev2->tag == Ev_IrNoX) {
               if (clo_cache_sim) {
                  helperName = "log_2IrNoX_0D_cache_access";
                  helperAddr = &log_2IrNoX_0D_cache_access;
               } else {
                  helperName = "log_2Ir";
                  helperAddr = &log_2Ir;
               }
               argv = mkIRExprVec_2( i_node_expr,
                                     mkIRExpr_HWord( (HWord)ev2->inode ) );
               regparms = 2;
               i += 2;
            }
            /* No merging possible; emit as-is. */
            else {
               if (clo_cache_sim) {
                  helperName = "log_1IrNoX_0D_cache_access";
                  helperAddr = &log_1IrNoX_0D_cache_access;
               } else {
                  helperName = "log_1Ir";
                  helperAddr = &log_1Ir;
               }
               argv = mkIRExprVec_1( i_node_expr );
               regparms = 1;
               i++;
            }
            break;
         case Ev_IrGen:
            if (clo_cache_sim) {
	       helperName = "log_1IrGen_0D_cache_access";
	       helperAddr = &log_1IrGen_0D_cache_access;
	    } else {
	       helperName = "log_1Ir";
	       helperAddr = &log_1Ir;
	    }
	    argv = mkIRExprVec_1( i_node_expr );
	    regparms = 1;
	    i++;
            break;
         case Ev_Dr:
         case Ev_Dm:
            /* Data read or modify */
            helperName = "log_0Ir_1Dr_cache_access";
            helperAddr = &log_0Ir_1Dr_cache_access;
            argv = mkIRExprVec_3( i_node_expr, 
                                  get_Event_dea(ev), 
                                  mkIRExpr_HWord( get_Event_dszB(ev) ) );
            regparms = 3;
            i++;
            break;
         case Ev_Dw:
            /* Data write */
            helperName = "log_0Ir_1Dw_cache_access";
            helperAddr = &log_0Ir_1Dw_cache_access;
            argv = mkIRExprVec_3( i_node_expr,
                                  get_Event_dea(ev), 
                                  mkIRExpr_HWord( get_Event_dszB(ev) ) );
            regparms = 3;
            i++;
            break;
         case Ev_Bc:
            /* Conditional branch */
            helperName = "log_cond_branch";
            helperAddr = &log_cond_branch;
            argv = mkIRExprVec_2( i_node_expr, ev->Ev.Bc.taken );
            regparms = 2;
            i++;
            break;
         case Ev_Bi:
            /* Branch to an unknown destination */
            helperName = "log_ind_branch";
            helperAddr = &log_ind_branch;
            argv = mkIRExprVec_2( i_node_expr, ev->Ev.Bi.dst );
            regparms = 2;
            i++;
            break;
         default:
            tl_assert(0);
      }

      /* Add the helper. */
      tl_assert(helperName);
      tl_assert(helperAddr);
      tl_assert(argv);
      di = unsafeIRDirty_0_N( regparms, 
                              helperName, VG_(fnptr_to_fnentry)( helperAddr ), 
                              argv );
      addStmtToIRSB( gds->sbOut, IRStmt_Dirty(di) );
   }

   gds->events_used = 0;
}

static void addEvent_Ir ( CdState* gds, InstrInfo* inode )
{
   Event* evt;
   if (gds->events_used == N_EVENTS)
      flushEvents(gds);
   tl_assert(gds->events_used >= 0 && gds->events_used < N_EVENTS);
   evt = &gds->events[gds->events_used];
   init_Event(evt);
   evt->inode    = inode;
   if (cachesim_is_IrNoX(inode->instr_addr, inode->instr_len)) {
      evt->tag = Ev_IrNoX;
      distinct_instrsNoX++;
   } else {
      evt->tag = Ev_IrGen;
      distinct_instrsGen++;
   }
   gds->events_used++;
}

static
void addEvent_Dr ( CdState* gds, InstrInfo* inode, Int datasize, IRAtom* ea )
{
   Event* evt;
   tl_assert(isIRAtom(ea));
   tl_assert(datasize >= 1 && datasize <= min_line_size);
   if (!clo_cache_sim)
      return;
   if (gds->events_used == N_EVENTS)
      flushEvents(gds);
   tl_assert(gds->events_used >= 0 && gds->events_used < N_EVENTS);
   evt = &gds->events[gds->events_used];
   init_Event(evt);
   evt->tag       = Ev_Dr;
   evt->inode     = inode;
   evt->Ev.Dr.szB = datasize;
   evt->Ev.Dr.ea  = ea;
   gds->events_used++;
}

static
void addEvent_Dw ( CdState* gds, InstrInfo* inode, Int datasize, IRAtom* ea )
{
   Event* evt;

   tl_assert(isIRAtom(ea));
   tl_assert(datasize >= 1 && datasize <= min_line_size);

   if (!clo_cache_sim)
      return;

   /* Is it possible to merge this write with the preceding read? */
   if (gds->events_used > 0) {
      Event* lastEvt = &gds->events[gds->events_used-1];
      if (   lastEvt->tag       == Ev_Dr
          && lastEvt->Ev.Dr.szB == datasize
          && lastEvt->inode     == inode
          && eqIRAtom(lastEvt->Ev.Dr.ea, ea))
      {
         lastEvt->tag   = Ev_Dm;
         return;
      }
   }

   /* No.  Add as normal. */
   if (gds->events_used == N_EVENTS)
      flushEvents(gds);
   tl_assert(gds->events_used >= 0 && gds->events_used < N_EVENTS);
   evt = &gds->events[gds->events_used];
   init_Event(evt);
   evt->tag       = Ev_Dw;
   evt->inode     = inode;
   evt->Ev.Dw.szB = datasize;
   evt->Ev.Dw.ea  = ea;
   gds->events_used++;
}

static
void addEvent_D_guarded ( CdState* gds, InstrInfo* inode,
                          Int datasize, IRAtom* ea, IRAtom* guard,
                          Bool isWrite )
{
   tl_assert(isIRAtom(ea));
   tl_assert(guard);
   tl_assert(isIRAtom(guard));
   tl_assert(datasize >= 1 && datasize <= min_line_size);

   if (!clo_cache_sim)
      return;

   /* Adding guarded memory actions and merging them with the existing
      queue is too complex.  Simply flush the queue and add this
      action immediately.  Since guarded loads and stores are pretty
      rare, this is not thought likely to cause any noticeable
      performance loss as a result of the loss of event-merging
      opportunities. */
   tl_assert(gds->events_used >= 0);
   flushEvents(gds);
   tl_assert(gds->events_used == 0);
   /* Same as case Ev_Dw / case Ev_Dr in flushEvents, except with guard */
   IRExpr*      i_node_expr;
   const HChar* helperName;
   void*        helperAddr;
   IRExpr**     argv;
   Int          regparms;
   IRDirty*     di;
   i_node_expr = mkIRExpr_HWord( (HWord)inode );
   helperName  = isWrite ? "log_0Ir_1Dw_cache_access"
                         : "log_0Ir_1Dr_cache_access";
   helperAddr  = isWrite ? &log_0Ir_1Dw_cache_access
                         : &log_0Ir_1Dr_cache_access;
   argv        = mkIRExprVec_3( i_node_expr,
                                ea, mkIRExpr_HWord( datasize ) );
   regparms    = 3;
   di          = unsafeIRDirty_0_N(
                    regparms, 
                    helperName, VG_(fnptr_to_fnentry)( helperAddr ), 
                    argv );
   di->guard = guard;
   addStmtToIRSB( gds->sbOut, IRStmt_Dirty(di) );
}


static
void addEvent_Bc ( CdState* gds, InstrInfo* inode, IRAtom* guard )
{
   Event* evt;
   tl_assert(isIRAtom(guard));
   tl_assert(typeOfIRExpr(gds->sbOut->tyenv, guard) 
             == (sizeof(RegWord)==4 ? Ity_I32 : Ity_I64));
   if (!clo_branch_sim)
      return;
   if (gds->events_used == N_EVENTS)
      flushEvents(gds);
   tl_assert(gds->events_used >= 0 && gds->events_used < N_EVENTS);
   evt = &gds->events[gds->events_used];
   init_Event(evt);
   evt->tag         = Ev_Bc;
   evt->inode       = inode;
   evt->Ev.Bc.taken = guard;
   gds->events_used++;
}

static
void addEvent_Bi ( CdState* gds, InstrInfo* inode, IRAtom* whereTo )
{
   Event* evt;
   tl_assert(isIRAtom(whereTo));
   tl_assert(typeOfIRExpr(gds->sbOut->tyenv, whereTo) 
             == (sizeof(RegWord)==4 ? Ity_I32 : Ity_I64));
   if (!clo_branch_sim)
      return;
   if (gds->events_used == N_EVENTS)
      flushEvents(gds);
   tl_assert(gds->events_used >= 0 && gds->events_used < N_EVENTS);
   evt = &gds->events[gds->events_used];
   init_Event(evt);
   evt->tag       = Ev_Bi;
   evt->inode     = inode;
   evt->Ev.Bi.dst = whereTo;
   gds->events_used++;
}

////////////////////////////////////////////////////////////

static
IRSB* cd_instrument ( VgCallbackClosure* closure,
                      IRSB* sbIn, 
                      const VexGuestLayout* layout, 
                      const VexGuestExtents* vge,
                      const VexArchInfo* archinfo_host,
                      IRType gWordTy, IRType hWordTy )
{
   Int        i;
   UInt       isize;
   IRStmt*    st;
   Addr       cia; /* address of current insn */
   CdState    gds;
   IRTypeEnv* tyenv = sbIn->tyenv;
   InstrInfo* curr_inode = NULL;

   if (gWordTy != hWordTy) {
      /* We don't currently support this case. */
      VG_(tool_panic)("host/guest word size mismatch");
   }

   // Set up new SB
   gds.sbOut = deepCopyIRSBExceptStmts(sbIn);

   // Copy verbatim any IR preamble preceding the first IMark
   i = 0;
   while (i < sbIn->stmts_used && sbIn->stmts[i]->tag != Ist_IMark) {
      addStmtToIRSB( gds.sbOut, sbIn->stmts[i] );
      i++;
   }

   // Get the first statement, and initial cia from it
   tl_assert(sbIn->stmts_used > 0);
   tl_assert(i < sbIn->stmts_used);
   st = sbIn->stmts[i];
   tl_assert(Ist_IMark == st->tag);

   cia   = st->Ist.IMark.addr;
   isize = st->Ist.IMark.len;
   // If Vex fails to decode an instruction, the size will be zero.
   // Pretend otherwise.
   if (isize == 0) isize = VG_MIN_INSTR_SZB;

   // Set up running state and get block info
   tl_assert(closure->readdr == vge->base[0]);
   gds.events_used = 0;
   gds.sbInfo      = get_SB_info(sbIn, (Addr)closure->readdr);
   gds.sbInfo_i    = 0;

   if (DEBUG_CG)
      VG_(printf)("\n\n---------- cd_instrument ----------\n");

   // Traverse the block, initialising inodes, adding events and flushing as
   // necessary.
   for (/*use current i*/; i < sbIn->stmts_used; i++) {

      st = sbIn->stmts[i];
      tl_assert(isFlatIRStmt(st));

      switch (st->tag) {
         case Ist_NoOp:
         case Ist_AbiHint:
         case Ist_Put:
         case Ist_PutI:
         case Ist_MBE:
            break;

         case Ist_IMark:
            cia   = st->Ist.IMark.addr;
            isize = st->Ist.IMark.len;

            // If Vex fails to decode an instruction, the size will be zero.
            // Pretend otherwise.
            if (isize == 0) isize = VG_MIN_INSTR_SZB;

            // Sanity-check size.
            tl_assert( (VG_MIN_INSTR_SZB <= isize && isize <= VG_MAX_INSTR_SZB)
                     || VG_CLREQ_SZB == isize );

            // Get space for and init the inode, record it as the current one.
            // Subsequent Dr/Dw/Dm events from the same instruction will 
            // also use it.
            curr_inode = setup_InstrInfo(&gds, cia, isize);

            addEvent_Ir( &gds, curr_inode );
            break;

         case Ist_WrTmp: {
            IRExpr* data = st->Ist.WrTmp.data;
            if (data->tag == Iex_Load) {
               IRExpr* aexpr = data->Iex.Load.addr;
               // Note also, endianness info is ignored.  I guess
               // that's not interesting.
               addEvent_Dr( &gds, curr_inode, sizeofIRType(data->Iex.Load.ty), 
                                  aexpr );
            }
            break;
         }

         case Ist_Store: {
            IRExpr* data  = st->Ist.Store.data;
            IRExpr* aexpr = st->Ist.Store.addr;
            addEvent_Dw( &gds, curr_inode, 
                         sizeofIRType(typeOfIRExpr(tyenv, data)), aexpr );
            break;
         }

         case Ist_StoreG: {
            IRStoreG* sg   = st->Ist.StoreG.details;
            IRExpr*   data = sg->data;
            IRExpr*   addr = sg->addr;
            IRType    type = typeOfIRExpr(tyenv, data);
            tl_assert(type != Ity_INVALID);
            addEvent_D_guarded( &gds, curr_inode,
                                sizeofIRType(type), addr, sg->guard,
                                True/*isWrite*/ );
            break;
         }

         case Ist_LoadG: {
            IRLoadG* lg       = st->Ist.LoadG.details;
            IRType   type     = Ity_INVALID; /* loaded type */
            IRType   typeWide = Ity_INVALID; /* after implicit widening */
            IRExpr*  addr     = lg->addr;
            typeOfIRLoadGOp(lg->cvt, &typeWide, &type);
            tl_assert(type != Ity_INVALID);
            addEvent_D_guarded( &gds, curr_inode,
                                sizeofIRType(type), addr, lg->guard,
                                False/*!isWrite*/ );
            break;
         }

         case Ist_Dirty: {
            Int      dataSize;
            IRDirty* d = st->Ist.Dirty.details;
            if (d->mFx != Ifx_None) {
               /* This dirty helper accesses memory.  Collect the details. */
               tl_assert(d->mAddr != NULL);
               tl_assert(d->mSize != 0);
               dataSize = d->mSize;
               // Large (eg. 28B, 108B, 512B on x86) data-sized
               // instructions will be done inaccurately, but they're
               // very rare and this avoids errors from hitting more
               // than two cache lines in the simulation.
               if (dataSize > min_line_size)
                  dataSize = min_line_size;
               if (d->mFx == Ifx_Read || d->mFx == Ifx_Modify)
                  addEvent_Dr( &gds, curr_inode, dataSize, d->mAddr );
               if (d->mFx == Ifx_Write || d->mFx == Ifx_Modify)
                  addEvent_Dw( &gds, curr_inode, dataSize, d->mAddr );
            } else {
               tl_assert(d->mAddr == NULL);
               tl_assert(d->mSize == 0);
            }
            break;
         }

         case Ist_CAS: {
            /* We treat it as a read and a write of the location.  I
               think that is the same behaviour as it was before IRCAS
               was introduced, since prior to that point, the Vex
               front ends would translate a lock-prefixed instruction
               into a (normal) read followed by a (normal) write. */
            Int    dataSize;
            IRCAS* cas = st->Ist.CAS.details;
            tl_assert(cas->addr != NULL);
            tl_assert(cas->dataLo != NULL);
            dataSize = sizeofIRType(typeOfIRExpr(tyenv, cas->dataLo));
            if (cas->dataHi != NULL)
               dataSize *= 2; /* since it's a doubleword-CAS */
            /* I don't think this can ever happen, but play safe. */
            if (dataSize > min_line_size)
               dataSize = min_line_size;
            addEvent_Dr( &gds, curr_inode, dataSize, cas->addr );
            addEvent_Dw( &gds, curr_inode, dataSize, cas->addr );
            break;
         }

         case Ist_LLSC: {
            IRType dataTy;
            if (st->Ist.LLSC.storedata == NULL) {
               /* LL */
               dataTy = typeOfIRTemp(tyenv, st->Ist.LLSC.result);
               addEvent_Dr( &gds, curr_inode,
                            sizeofIRType(dataTy), st->Ist.LLSC.addr );
               /* flush events before LL, should help SC to succeed */
               flushEvents( &gds );
            } else {
               /* SC */
               dataTy = typeOfIRExpr(tyenv, st->Ist.LLSC.storedata);
               addEvent_Dw( &gds, curr_inode,
                            sizeofIRType(dataTy), st->Ist.LLSC.addr );
            }
            break;
         }

         case Ist_Exit: {
            // call branch predictor only if this is a branch in guest code
            if ( (st->Ist.Exit.jk == Ijk_Boring) ||
                 (st->Ist.Exit.jk == Ijk_Call) ||
                 (st->Ist.Exit.jk == Ijk_Ret) )
            {
               /* Stuff to widen the guard expression to a host word, so
                  we can pass it to the branch predictor simulation
                  functions easily. */
               Bool     inverted;
               Addr     nia, sea;
               IRConst* dst;
               IRType   tyW    = hWordTy;
               IROp     widen  = tyW==Ity_I32  ? Iop_1Uto32  : Iop_1Uto64;
               IROp     opXOR  = tyW==Ity_I32  ? Iop_Xor32   : Iop_Xor64;
               IRTemp   guard1 = newIRTemp(gds.sbOut->tyenv, Ity_I1);
               IRTemp   guardW = newIRTemp(gds.sbOut->tyenv, tyW);
               IRTemp   guard  = newIRTemp(gds.sbOut->tyenv, tyW);
               IRExpr*  one    = tyW==Ity_I32 ? IRExpr_Const(IRConst_U32(1))
                                              : IRExpr_Const(IRConst_U64(1));

               /* First we need to figure out whether the side exit got
                  inverted by the ir optimiser.  To do that, figure out
                  the next (fallthrough) instruction's address and the
                  side exit address and see if they are the same. */
               nia = cia + isize;

               /* Side exit address */
               dst = st->Ist.Exit.dst;
               if (tyW == Ity_I32) {
                  tl_assert(dst->tag == Ico_U32);
                  sea = dst->Ico.U32;
               } else {
                  tl_assert(tyW == Ity_I64);
                  tl_assert(dst->tag == Ico_U64);
                  sea = dst->Ico.U64;
               }

               inverted = nia == sea;

               /* Widen the guard expression. */
               addStmtToIRSB( gds.sbOut,
                              IRStmt_WrTmp( guard1, st->Ist.Exit.guard ));
               addStmtToIRSB( gds.sbOut,
                              IRStmt_WrTmp( guardW,
                                            IRExpr_Unop(widen,
                                                        IRExpr_RdTmp(guard1))) );
               /* If the exit is inverted, invert the sense of the guard. */
               addStmtToIRSB(
                     gds.sbOut,
                     IRStmt_WrTmp(
                           guard,
                           inverted ? IRExpr_Binop(opXOR, IRExpr_RdTmp(guardW), one)
                                    : IRExpr_RdTmp(guardW)
                              ));
               /* And post the event. */
               addEvent_Bc( &gds, curr_inode, IRExpr_RdTmp(guard) );
            }

            /* We may never reach the next statement, so need to flush
               all outstanding transactions now. */
            flushEvents( &gds );
            break;
         }

         default:
            ppIRStmt(st);
            tl_assert(0);
            break;
      }

      /* Copy the original statement */
      addStmtToIRSB( gds.sbOut, st );

      if (DEBUG_CG) {
         ppIRStmt(st);
         VG_(printf)("\n");
      }
   }

   /* Deal with branches to unknown destinations.  Except ignore ones
      which are function returns as we assume the return stack
      predictor never mispredicts. */
   if ((sbIn->jumpkind == Ijk_Boring) || (sbIn->jumpkind == Ijk_Call)) {
      if (0) { ppIRExpr( sbIn->next ); VG_(printf)("\n"); }
      switch (sbIn->next->tag) {
         case Iex_Const: 
            break; /* boring - branch to known address */
         case Iex_RdTmp: 
            /* looks like an indirect branch (branch to unknown) */
            addEvent_Bi( &gds, curr_inode, sbIn->next );
            break;
         default:
            /* shouldn't happen - if the incoming IR is properly
               flattened, should only have tmp and const cases to
               consider. */
            tl_assert(0); 
      }
   }

   /* At the end of the bb.  Flush outstandings. */
   flushEvents( &gds );

   /* done.  stay sane ... */
   tl_assert(gds.sbInfo_i == gds.sbInfo->n_instrs);

   if (DEBUG_CG) {
      VG_(printf)( "goto {");
      ppIRJumpKind(sbIn->jumpkind);
      VG_(printf)( "} ");
      ppIRExpr( sbIn->next );
      VG_(printf)( "}\n");
   }

   return gds.sbOut;
}

/*------------------------------------------------------------*/
/*--- Cache configuration                                  ---*/
/*------------------------------------------------------------*/

static cache_t clo_I1_cache = UNDEFINED_CACHE;
static cache_t clo_D1_cache = UNDEFINED_CACHE;
static cache_t clo_LL_cache = UNDEFINED_CACHE;

/*------------------------------------------------------------*/
/*--- cd_fini() and related function                       ---*/
/*------------------------------------------------------------*/

// Total reads/writes/misses.  Calculated during CC traversal at the end.
// All auto-zeroed.
static CacheCC  Ir_total;
static CacheCC  Dr_total;
static CacheCC  Dw_total;
static BranchCC Bc_total;
static BranchCC Bi_total;

static void fprint_CC_table_and_calc_totals(void)
{
   Int     i;
   VgFile  *fp;
   HChar   *currFile = NULL;
   const HChar *currFn = NULL;
   LineCC* lineCC;

   // Setup output filename.  Nb: it's important to do this now, ie. as late
   // as possible.  If we do it at start-up and the program forks and the
   // output file format string contains a %p (pid) specifier, both the
   // parent and child will incorrectly write to the same file;  this
   // happened in 3.3.0.
   HChar* cachegrind_out_file =
      VG_(expand_file_name)("--cachegrind-out-file", clo_cachegrind_out_file);

   fp = VG_(fopen)(cachegrind_out_file, VKI_O_CREAT|VKI_O_TRUNC|VKI_O_WRONLY,
                                        VKI_S_IRUSR|VKI_S_IWUSR);
   if (fp == NULL) {
      // If the file can't be opened for whatever reason (conflict
      // between multiple cachegrinded processes?), give up now.
      VG_(umsg)("error: can't open cache simulation output file '%s'\n",
                cachegrind_out_file );
      VG_(umsg)("       ... so simulation results will be missing.\n");
      VG_(free)(cachegrind_out_file);
      return;
   } else {
      VG_(free)(cachegrind_out_file);
   }

   // "desc:" lines (giving I1/D1/LL cache configuration).  The spaces after
   // the 2nd colon makes cd_annotate's output look nicer.
   VG_(fprintf)(fp,  "desc: I1 cache:         %s\n"
                     "desc: D1 cache:         %s\n"
                     "desc: LL cache:         %s\n",
                     I1.desc_line, D1.desc_line, LL.desc_line);

   // "cmd:" line
   VG_(fprintf)(fp, "cmd: %s", VG_(args_the_exename));
   for (i = 0; i < VG_(sizeXA)( VG_(args_for_client) ); i++) {
      HChar* arg = * (HChar**) VG_(indexXA)( VG_(args_for_client), i );
      VG_(fprintf)(fp, " %s", arg);
   }
   // "events:" line
   if (clo_cache_sim && clo_branch_sim) {
      VG_(fprintf)(fp, "\nevents: Ir I1mr ILmr Dr D1mr DLmr Dw D1mw DLmw "
                                  "Bc Bcm Bi Bim\n");
   }
   else if (clo_cache_sim && !clo_branch_sim) {
      VG_(fprintf)(fp, "\nevents: Ir I1mr ILmr Dr D1mr DLmr Dw D1mw DLmw "
                                  "\n");
   }
   else if (!clo_cache_sim && clo_branch_sim) {
      VG_(fprintf)(fp, "\nevents: Ir Bc Bcm Bi Bim\n");
   }
   else {
      VG_(fprintf)(fp, "\nevents: Ir\n");
   }

   // Traverse every lineCC
   VG_(OSetGen_ResetIter)(CC_table);
   while ( (lineCC = VG_(OSetGen_Next)(CC_table)) ) {
      Bool just_hit_a_new_file = False;
      // If we've hit a new file, print a "fl=" line.  Note that because
      // each string is stored exactly once in the string table, we can use
      // pointer comparison rather than strcmp() to test for equality, which
      // is good because most of the time the comparisons are equal and so
      // the whole strings would have to be checked.
      if ( lineCC->loc.file != currFile ) {
         currFile = lineCC->loc.file;
         VG_(fprintf)(fp, "fl=%s\n", currFile);
         distinct_files++;
         just_hit_a_new_file = True;
      }
      // If we've hit a new function, print a "fn=" line.  We know to do
      // this when the function name changes, and also every time we hit a
      // new file (in which case the new function name might be the same as
      // in the old file, hence the just_hit_a_new_file test).
      if ( just_hit_a_new_file || lineCC->loc.fn != currFn ) {
         currFn = lineCC->loc.fn;
         VG_(fprintf)(fp, "fn=%s\n", currFn);
         distinct_fns++;
      }

      // Print the LineCC
      if (clo_cache_sim && clo_branch_sim) {
         VG_(fprintf)(fp,  "%d %llu %llu %llu"
                             " %llu %llu %llu"
                             " %llu %llu %llu"
                             " %llu %llu %llu %llu\n",
                            lineCC->loc.line,
                            lineCC->Ir.a, lineCC->Ir.m1, lineCC->Ir.mL, 
                            lineCC->Dr.a, lineCC->Dr.m1, lineCC->Dr.mL,
                            lineCC->Dw.a, lineCC->Dw.m1, lineCC->Dw.mL,
                            lineCC->Bc.b, lineCC->Bc.mp, 
                            lineCC->Bi.b, lineCC->Bi.mp);
      }
      else if (clo_cache_sim && !clo_branch_sim) {
         VG_(fprintf)(fp,  "%d %llu %llu %llu"
                             " %llu %llu %llu"
                             " %llu %llu %llu\n",
                            lineCC->loc.line,
                            lineCC->Ir.a, lineCC->Ir.m1, lineCC->Ir.mL, 
                            lineCC->Dr.a, lineCC->Dr.m1, lineCC->Dr.mL,
                            lineCC->Dw.a, lineCC->Dw.m1, lineCC->Dw.mL);
      }
      else if (!clo_cache_sim && clo_branch_sim) {
         VG_(fprintf)(fp,  "%d %llu"
                             " %llu %llu %llu %llu\n",
                            lineCC->loc.line,
                            lineCC->Ir.a, 
                            lineCC->Bc.b, lineCC->Bc.mp, 
                            lineCC->Bi.b, lineCC->Bi.mp);
      }
      else {
         VG_(fprintf)(fp,  "%d %llu\n",
                            lineCC->loc.line,
                            lineCC->Ir.a);
      }

      // Update summary stats
      Ir_total.a  += lineCC->Ir.a;
      Ir_total.m1 += lineCC->Ir.m1;
      Ir_total.mL += lineCC->Ir.mL;
      Dr_total.a  += lineCC->Dr.a;
      Dr_total.m1 += lineCC->Dr.m1;
      Dr_total.mL += lineCC->Dr.mL;
      Dw_total.a  += lineCC->Dw.a;
      Dw_total.m1 += lineCC->Dw.m1;
      Dw_total.mL += lineCC->Dw.mL;
      Bc_total.b  += lineCC->Bc.b;
      Bc_total.mp += lineCC->Bc.mp;
      Bi_total.b  += lineCC->Bi.b;
      Bi_total.mp += lineCC->Bi.mp;

      distinct_lines++;
   }

   // Summary stats must come after rest of table, since we calculate them
   // during traversal.  */
   if (clo_cache_sim && clo_branch_sim) {
      VG_(fprintf)(fp,  "summary:"
                        " %llu %llu %llu"
                        " %llu %llu %llu"
                        " %llu %llu %llu"
                        " %llu %llu %llu %llu\n", 
                        Ir_total.a, Ir_total.m1, Ir_total.mL,
                        Dr_total.a, Dr_total.m1, Dr_total.mL,
                        Dw_total.a, Dw_total.m1, Dw_total.mL,
                        Bc_total.b, Bc_total.mp, 
                        Bi_total.b, Bi_total.mp);
   }
   else if (clo_cache_sim && !clo_branch_sim) {
      VG_(fprintf)(fp,  "summary:"
                        " %llu %llu %llu"
                        " %llu %llu %llu"
                        " %llu %llu %llu\n",
                        Ir_total.a, Ir_total.m1, Ir_total.mL,
                        Dr_total.a, Dr_total.m1, Dr_total.mL,
                        Dw_total.a, Dw_total.m1, Dw_total.mL);
   }
   else if (!clo_cache_sim && clo_branch_sim) {
      VG_(fprintf)(fp,  "summary:"
                        " %llu"
                        " %llu %llu %llu %llu\n", 
                        Ir_total.a,
                        Bc_total.b, Bc_total.mp, 
                        Bi_total.b, Bi_total.mp);
   }
   else {
      VG_(fprintf)(fp, "summary:"
                        " %llu\n", 
                        Ir_total.a);
   }

   VG_(fclose)(fp);
}

static UInt ULong_width(ULong n)
{
   UInt w = 0;
   while (n > 0) {
      n = n / 10;
      w++;
   }
   if (w == 0) w = 1;
   return w + (w-1)/3;   // add space for commas
}

static VgFile* fp_heap;
static VgFile* fp_heap_hm_w;
static VgFile* fp_heap_hm_r;

#define FP(format, args...) ({ VG_(fprintf)(fp_heap, format, ##args); })
#define FHHW(format, args...) ({ VG_(fprintf)(fp_heap_hm_w, format, ##args); })
#define FHHR(format, args...) ({ VG_(fprintf)(fp_heap_hm_r, format, ##args); })

// The frame table holds unique frames.
static WordFM* frame_tbl = NULL;
static UWord next_frame_n = 0;

static Word frame_cmp(UWord a, UWord b)
{
   return VG_(strcmp)((const HChar*)a, (const HChar*)b);
}

static HChar hex_digit_to_ascii_char(UChar d)
{
   d = d & 0xf;
   return (d < 10) ? ('0' + d) : ('a' + (d - 10));
}

// For JSON, we must escape double quote, backslash, and 0x00..0x1f.
//
// Returns the original string if no escaping was required. Returns a pointer
// to a static buffer if escaping was required. Therefore, the return value is
// only valid until the next call to this function.
static const HChar* json_escape(const HChar* s)
{
   static HChar* buf = NULL;
   static SizeT bufcap = 0;

   // Do we need any escaping?
   SizeT extra = 0;
   const HChar* p = s;
   while (*p) {
      UChar c = *p;
      if (c == '"' || c == '\\') {
         extra += 1;
      } else if (c <= 0x1f) {
         extra += 5;
      }
      p++;
   }
   SizeT len = p - s;

   if (extra == 0) {
      // No escaping needed.
      return s;
   }

   // Escaping needed. (The +1 is for the NUL terminator.) Enlarge buf if
   // necessary.
   SizeT newcap = len + extra + 1;
   if (bufcap < newcap) {
      buf = VG_(realloc)("dh.json", buf, newcap);
      bufcap = newcap;
   }

   p = s;
   HChar* q = buf;
   while (*p) {
      UChar c = *p;
      if (c == '"') {
         *q++ = '\\';
         *q++ = '"';
      } else if (c == '\\') {
         *q++ = '\\';
         *q++ = '\\';
      } else if (c <= 0x1f) {
         *q++ = '\\';
         *q++ = 'u';
         *q++ = '0';
         *q++ = '0';
         *q++ = hex_digit_to_ascii_char((c & 0x00f0) >> 4);
         *q++ = hex_digit_to_ascii_char(c & 0x000f);
      } else {
         *q++ = c;
      }
      p++;
   }
   *q = '\0';

   return buf;
}

static void write_APInfo_frame(UInt n, DiEpoch ep, Addr ip, void* opaque)
{
   Bool* is_first = (Bool*)opaque;
   InlIPCursor* iipc = VG_(new_IIPC)(ep, ip);

   do {
      const HChar* buf = VG_(describe_IP)(ep, ip, iipc);

      // Skip entries in vg_replace_malloc.c (e.g. `malloc`, `calloc`,
      // `realloc`, `operator new`) because they're boring and clog up the
      // output.
      if (VG_(strstr)(buf, "vg_replace_malloc.c")) {
         continue;
      }

      // If this description has been seen before, get its number. Otherwise,
      // give it a new number and put it in the table.
      UWord keyW = 0, valW = 0;
      UWord frame_n = 0;
      Bool found = VG_(lookupFM)(frame_tbl, &keyW, &valW, (UWord)buf);
      if (found) {
         //const HChar* str = (const HChar*)keyW;
         //tl_assert(0 == VG_(strcmp)(buf, str));
         frame_n = valW;
      } else {
         // `buf` is a static buffer, we must copy it.
         const HChar* str = VG_(strdup)("dh.frame_tbl.3", buf);
         frame_n = next_frame_n++;
         Bool present = VG_(addToFM)(frame_tbl, (UWord)str, frame_n);
         tl_assert(!present);
      }

      FP("%c%lu", *is_first ? '[' : ',', frame_n);
      *is_first = False;

   } while (VG_(next_IIPC)(iipc));

   VG_(delete_IIPC)(iipc);
};

static void write_APInfo_frameW(UInt n, DiEpoch ep, Addr ip, void* opaque)
{
   Bool* is_first = (Bool*)opaque;
   InlIPCursor* iipc = VG_(new_IIPC)(ep, ip);

   do {
      const HChar* buf = VG_(describe_IP)(ep, ip, iipc);

      // Skip entries in vg_replace_malloc.c (e.g. `malloc`, `calloc`,
      // `realloc`, `operator new`) because they're boring and clog up the
      // output.
      if (VG_(strstr)(buf, "vg_replace_malloc.c")) {
         continue;
      }

      // If this description has been seen before, get its number. Otherwise,
      // give it a new number and put it in the table.
      UWord keyW = 0, valW = 0;
      UWord frame_n = 0;
      Bool found = VG_(lookupFM)(frame_tbl, &keyW, &valW, (UWord)buf);
      if (found) {
         //const HChar* str = (const HChar*)keyW;
         //tl_assert(0 == VG_(strcmp)(buf, str));
         frame_n = valW;
      } else {
         // `buf` is a static buffer, we must copy it.
         const HChar* str = VG_(strdup)("dh.frame_tbl.3", buf);
         frame_n = next_frame_n++;
         Bool present = VG_(addToFM)(frame_tbl, (UWord)str, frame_n);
         tl_assert(!present);
      }

      if (clo_hm_write) FHHW("%c%lu", *is_first ? '[' : ',', frame_n);
      *is_first = False;

   } while (VG_(next_IIPC)(iipc));

   VG_(delete_IIPC)(iipc);
};

static void write_APInfo_frameR(UInt n, DiEpoch ep, Addr ip, void* opaque)
{
   Bool* is_first = (Bool*)opaque;
   InlIPCursor* iipc = VG_(new_IIPC)(ep, ip);

   do {
      const HChar* buf = VG_(describe_IP)(ep, ip, iipc);

      // Skip entries in vg_replace_malloc.c (e.g. `malloc`, `calloc`,
      // `realloc`, `operator new`) because they're boring and clog up the
      // output.
      if (VG_(strstr)(buf, "vg_replace_malloc.c")) {
         continue;
      }

      // If this description has been seen before, get its number. Otherwise,
      // give it a new number and put it in the table.
      UWord keyW = 0, valW = 0;
      UWord frame_n = 0;
      Bool found = VG_(lookupFM)(frame_tbl, &keyW, &valW, (UWord)buf);
      if (found) {
         //const HChar* str = (const HChar*)keyW;
         //tl_assert(0 == VG_(strcmp)(buf, str));
         frame_n = valW;
      } else {
         // `buf` is a static buffer, we must copy it.
         const HChar* str = VG_(strdup)("dh.frame_tbl.3", buf);
         frame_n = next_frame_n++;
         Bool present = VG_(addToFM)(frame_tbl, (UWord)str, frame_n);
         tl_assert(!present);
      }

      if (clo_hm_read) FHHR("%c%lu", *is_first ? '[' : ',', frame_n);
      *is_first = False;

   } while (VG_(next_IIPC)(iipc));

   VG_(delete_IIPC)(iipc);
};

static void write_APInfo(APInfo* api, Bool is_first)
{
   tl_assert(api->total_blocks >= api->max_blocks);
   tl_assert(api->total_bytes >= api->max_bytes);

   FP(" %c{\"tb\":%llu,\"tbk\":%llu,\"tli\":%llu\n",
      is_first ? '[' : ',',
      api->total_bytes, api->total_blocks, api->total_lifetimes_instrs);
   FP("  ,\"mb\":%llu,\"mbk\":%llu\n",
      api->max_bytes, api->max_blocks);
   FP("  ,\"gb\":%llu,\"gbk\":%llu\n",
      api->at_tgmax_bytes, api->at_tgmax_blocks);
   FP("  ,\"fb\":%llu,\"fbk\":%llu\n",
      api->curr_bytes, api->curr_blocks);
   FP("  ,\"rb\":%llu,\"wb\":%llu\n",
      api->reads_bytes, api->writes_bytes);

   if (api->histo && api->xsize_tag == Exactly) {
      FP("  ,\"acc\":[");

      // Simple run-length encoding: when N entries in a row have the same
      // value M, we print "-N,M". If there is just one in a row, we just
      // print "M". This reduces file size significantly.
      UShort repval = 0;
      Int reps = 0;
      for (UWord i = 0; i < api->xsize; i++) {
         UShort h = api->histo[i];
         if (repval == h) {
            // Continue current run.
            reps++;
         } else {
            // End of run; print it.
            if (reps == 1) {
               FP("%u,", repval);
            } else if (reps > 1) {
               FP("-%d,%u,", reps, repval);
            }
            reps = 1;
            repval = h;
         }
      }
      // Print the final run.
      if (reps == 1) {
         FP("%u", repval);
      } else if (reps > 1) {
         FP("-%d,%u", reps, repval);
      }

      FP("]\n");
   }

   FP("  ,\"fs\":");
   Bool is_first_frame = True;
   VG_(apply_ExeContext)(write_APInfo_frame, &is_first_frame, api->ap);
   FP("]\n");

   FP("  }\n");
}

static void write_APInfos(void)
{
   UWord keyW, valW;

   FP(",\"aps\":\n");

   VG_(initIterFM)(apinfo);
   Bool is_first = True;
   while (VG_(nextIterFM)(apinfo, &keyW, &valW)) {
      APInfo* api = (APInfo*)valW;
      tl_assert(api && api->ap == (ExeContext*)keyW);
      write_APInfo(api, is_first);
      is_first = False;
   }
   VG_(doneIterFM)(apinfo);

   if (is_first) {
      // We didn't print any elements. This happens if apinfo is empty.
      FP(" [\n");
   }

   FP(" ]\n");
}


static void write_AP_HM(APInfo* api, Bool is_first)
{
   tl_assert(api->total_blocks >= api->max_blocks);
   tl_assert(api->total_bytes >= api->max_bytes);

   if (clo_hm_write && api->writes_bytes >= clo_hm_writes_limit) {
      FHHW("w_fs: ");
      Bool is_first_frame = True;
      VG_(apply_ExeContext)(write_APInfo_frameW, &is_first_frame, api->ap);
      FHHW("]\n");
   }

   if (clo_hm_read && api->reads_bytes >= clo_hm_reads_limit) {
      FHHR("r_fs: ");
      Bool is_first_frame = True;
      VG_(apply_ExeContext)(write_APInfo_frameR, &is_first_frame, api->ap);
      FHHR("]\n");
   }

   HeatMap* hm_node = api->HMHead->next; // dummy head for api

   while(hm_node != NULL) {
      for (UInt i = 0; i < hm_node->mem_region_size; i++) {
         if (clo_hm_write && api->writes_bytes >= clo_hm_writes_limit) FHHW("%d\t", hm_node->mem_region_w[i]);
         if (clo_hm_read && api->reads_bytes >= clo_hm_reads_limit) FHHR("%d\t", hm_node->mem_region_r[i]);
      }
      if (clo_hm_write && api->writes_bytes >= clo_hm_writes_limit) FHHW("\n");
      if (clo_hm_read && api->reads_bytes >= clo_hm_reads_limit) FHHR("\n");
      //VG_(printf)("print head %p, hm_node %p\n", api->HMHead, hm_node);
      hm_node = hm_node->next;
   }
}

static void write_AP_HMs(void)
{
   UWord keyW, valW;

   if (clo_hm_write) FHHW("ts-res: %d\n", clo_ts_res);
   if (clo_hm_write) FHHW("mem-res: %d\n", clo_mem_res);
   if (clo_hm_write) FHHW("hm-size-limit: %d\n", clo_hm_size_limit);

   if (clo_hm_read) FHHR("ts-res: %d\n", clo_ts_res);
   if (clo_hm_read) FHHR("mem-res: %d\n", clo_mem_res);
   if (clo_hm_read) FHHR("hm-size-limit: %d\n", clo_hm_size_limit);

   VG_(initIterFM)(apinfo);
   Bool is_first = True;
   while (VG_(nextIterFM)(apinfo, &keyW, &valW)) {
      APInfo* api = (APInfo*)valW;
      tl_assert(api && api->ap == (ExeContext*)keyW);
      if(api->HMHead != api->HMNode) {
         write_AP_HM(api, is_first);
         is_first = False;
      }
   }
   VG_(doneIterFM)(apinfo);
}

static void cd_fini(Int exitcode)
{
   static HChar fmt[128];   // OK; large enough

   CacheCC  D_total;
   BranchCC B_total;
   ULong LL_total_m, LL_total_mr, LL_total_mw,
         LL_total, LL_total_r, LL_total_w;
   Int l1, l2, l3;

   fprint_CC_table_and_calc_totals();

   if (VG_(clo_verbosity) == 0) 
      return;

   // Nb: this isn't called "MAX" because that overshadows a global on Darwin.
   #define CD_MAX(a, b)  ((a) >= (b) ? (a) : (b))

   /* I cache results.  Use the I_refs value to determine the first column
    * width. */
   l1 = ULong_width(Ir_total.a);
   l2 = ULong_width(CD_MAX(Dr_total.a, Bc_total.b));
   l3 = ULong_width(CD_MAX(Dw_total.a, Bi_total.b));

   /* Make format string, getting width right for numbers */
   VG_(sprintf)(fmt, "%%s %%,%dllu\n", l1);

   /* Always print this */
   VG_(umsg)(fmt, "I   refs:     ", Ir_total.a);

   /* If cache profiling is enabled, show D access numbers and all
      miss numbers */
   if (clo_cache_sim) {
      VG_(umsg)(fmt, "I1  misses:   ", Ir_total.m1);
      VG_(umsg)(fmt, "LLi misses:   ", Ir_total.mL);

      if (0 == Ir_total.a) Ir_total.a = 1;
      VG_(umsg)("I1  miss rate: %*.2f%%\n", l1,
                Ir_total.m1 * 100.0 / Ir_total.a);
      VG_(umsg)("LLi miss rate: %*.2f%%\n", l1,
                Ir_total.mL * 100.0 / Ir_total.a);
      VG_(umsg)("\n");

      /* D cache results.  Use the D_refs.rd and D_refs.wr values to
       * determine the width of columns 2 & 3. */
      D_total.a  = Dr_total.a  + Dw_total.a;
      D_total.m1 = Dr_total.m1 + Dw_total.m1;
      D_total.mL = Dr_total.mL + Dw_total.mL;

      /* Make format string, getting width right for numbers */
      VG_(sprintf)(fmt, "%%s %%,%dllu  (%%,%dllu rd   + %%,%dllu wr)\n",
                        l1, l2, l3);

      VG_(umsg)(fmt, "D   refs:     ", 
                     D_total.a, Dr_total.a, Dw_total.a);
      VG_(umsg)(fmt, "D1  misses:   ",
                     D_total.m1, Dr_total.m1, Dw_total.m1);
      VG_(umsg)(fmt, "LLd misses:   ",
                     D_total.mL, Dr_total.mL, Dw_total.mL);

      if (0 == D_total.a)  D_total.a = 1;
      if (0 == Dr_total.a) Dr_total.a = 1;
      if (0 == Dw_total.a) Dw_total.a = 1;
      VG_(umsg)("D1  miss rate: %*.1f%% (%*.1f%%     + %*.1f%%  )\n",
                l1, D_total.m1  * 100.0 / D_total.a,
                l2, Dr_total.m1 * 100.0 / Dr_total.a,
                l3, Dw_total.m1 * 100.0 / Dw_total.a);
      VG_(umsg)("LLd miss rate: %*.1f%% (%*.1f%%     + %*.1f%%  )\n",
                l1, D_total.mL  * 100.0 / D_total.a,
                l2, Dr_total.mL * 100.0 / Dr_total.a,
                l3, Dw_total.mL * 100.0 / Dw_total.a);
      VG_(umsg)("\n");

      /* LL overall results */

      LL_total   = Dr_total.m1 + Dw_total.m1 + Ir_total.m1;
      LL_total_r = Dr_total.m1 + Ir_total.m1;
      LL_total_w = Dw_total.m1;
      VG_(umsg)(fmt, "LL refs:      ",
                     LL_total, LL_total_r, LL_total_w);

      LL_total_m  = Dr_total.mL + Dw_total.mL + Ir_total.mL;
      LL_total_mr = Dr_total.mL + Ir_total.mL;
      LL_total_mw = Dw_total.mL;
      VG_(umsg)(fmt, "LL misses:    ",
                     LL_total_m, LL_total_mr, LL_total_mw);

      VG_(umsg)("LL miss rate:  %*.1f%% (%*.1f%%     + %*.1f%%  )\n",
                l1, LL_total_m  * 100.0 / (Ir_total.a + D_total.a),
                l2, LL_total_mr * 100.0 / (Ir_total.a + Dr_total.a),
                l3, LL_total_mw * 100.0 / Dw_total.a);
      
   }

   /* If branch profiling is enabled, show branch overall results. */
   if (clo_branch_sim) {
      /* Make format string, getting width right for numbers */
      VG_(sprintf)(fmt, "%%s %%,%dllu  (%%,%dllu cond + %%,%dllu ind)\n",
                        l1, l2, l3);

      if (0 == Bc_total.b)  Bc_total.b = 1;
      if (0 == Bi_total.b)  Bi_total.b = 1;
      B_total.b  = Bc_total.b  + Bi_total.b;
      B_total.mp = Bc_total.mp + Bi_total.mp;

      VG_(umsg)("\n");
      VG_(umsg)(fmt, "Branches:     ",
                     B_total.b, Bc_total.b, Bi_total.b);

      VG_(umsg)(fmt, "Mispredicts:  ",
                     B_total.mp, Bc_total.mp, Bi_total.mp);

      VG_(umsg)("Mispred rate:  %*.1f%% (%*.1f%%     + %*.1f%%   )\n",
                l1, B_total.mp  * 100.0 / B_total.b,
                l2, Bc_total.mp * 100.0 / Bc_total.b,
                l3, Bi_total.mp * 100.0 / Bi_total.b);
   }

   // Various stats
   if (VG_(clo_stats)) {
      Int debug_lookups = full_debugs      + fn_debugs +
                          file_line_debugs + no_debugs;

      VG_(dmsg)("\n");
      VG_(dmsg)("cachegrind: distinct files     : %d\n", distinct_files);
      VG_(dmsg)("cachegrind: distinct functions : %d\n", distinct_fns);
      VG_(dmsg)("cachegrind: distinct lines     : %d\n", distinct_lines);
      VG_(dmsg)("cachegrind: distinct instrs NoX: %d\n", distinct_instrsNoX);
      VG_(dmsg)("cachegrind: distinct instrs Gen: %d\n", distinct_instrsGen);
      VG_(dmsg)("cachegrind: debug lookups      : %d\n", debug_lookups);
      
      VG_(dmsg)("cachegrind: with full      info:%6.1f%% (%d)\n", 
                full_debugs * 100.0 / debug_lookups, full_debugs);
      VG_(dmsg)("cachegrind: with file/line info:%6.1f%% (%d)\n", 
                file_line_debugs * 100.0 / debug_lookups, file_line_debugs);
      VG_(dmsg)("cachegrind: with fn name   info:%6.1f%% (%d)\n", 
                fn_debugs * 100.0 / debug_lookups, fn_debugs);
      VG_(dmsg)("cachegrind: with zero      info:%6.1f%% (%d)\n", 
                no_debugs * 100.0 / debug_lookups, no_debugs);

      VG_(dmsg)("cachegrind: string table size: %u\n",
                VG_(OSetGen_Size)(stringTable));
      VG_(dmsg)("cachegrind: CC table size: %u\n",
                VG_(OSetGen_Size)(CC_table));
      VG_(dmsg)("cachegrind: InstrInfo table size: %u\n",
                VG_(OSetGen_Size)(instrInfoTable));
   }


   // Malloc profile print
   const HChar* clo_dhat_out_file = "heapProfile.out.%p";
   const HChar* clo_hm_out_file_w = "heapProfileHMW.out.%p";
   const HChar* clo_hm_out_file_r = "heapProfileHMR.out.%p";

   // Total bytes might be at a possible peak.
   check_for_peak();

   // Before printing statistics, we must harvest various stats (such as
   // lifetimes and accesses) for all the blocks that are still alive.
   UWord keyW, valW;
   VG_(initIterFM)( interval_tree );
   while (VG_(nextIterFM)( interval_tree, &keyW, &valW )) {
      Block* bk = (Block*)keyW;
      tl_assert(valW == 0);
      tl_assert(bk);
      retire_Block(bk, False/*!because_freed*/);
   }
   VG_(doneIterFM)( interval_tree );

   // Stats.
   if (VG_(clo_stats)) {
      VG_(dmsg)(" dhat: find_Block_containing:\n");
      VG_(dmsg)("             found: %'lu (%'lu cached + %'lu uncached)\n",
                stats__n_fBc_cached + stats__n_fBc_uncached,
                stats__n_fBc_cached,
                stats__n_fBc_uncached);
      VG_(dmsg)("          notfound: %'lu\n", stats__n_fBc_notfound);
      VG_(dmsg)("\n");
   }

   // Create the frame table, and insert the special "[root]" node at index 0.
   frame_tbl = VG_(newFM)(VG_(malloc),
                          "dh.frame_tbl.1",
                          VG_(free),
                          frame_cmp);
   const HChar* root = VG_(strdup)("dh.frame_tbl.2", "[root]");
   Bool present = VG_(addToFM)(frame_tbl, (UWord)root, 0);
   tl_assert(!present);
   next_frame_n = 1;

   // Setup output filename. Nb: it's important to do this now, i.e. as late
   // as possible. If we do it at start-up and the program forks and the
   // output file format string contains a %p (pid) specifier, both the parent
   // and child will incorrectly write to the same file; this happened in
   // 3.3.0.
   HChar* dhat_out_file =
      VG_(expand_file_name)("--dhat-out-file", clo_dhat_out_file);
   HChar* dhat_hm_out_file_w =
      VG_(expand_file_name)("--dhat-out-file", clo_hm_out_file_w);
   HChar* dhat_hm_out_file_r =
      VG_(expand_file_name)("--dhat-out-file", clo_hm_out_file_r);

   fp_heap = VG_(fopen)(dhat_out_file, VKI_O_CREAT|VKI_O_TRUNC|VKI_O_WRONLY,
                   VKI_S_IRUSR|VKI_S_IWUSR);
   if (!fp_heap) {
      VG_(umsg)("error: can't open DHAT output file '%s'\n", dhat_out_file);
      return;
   }

   if (clo_hm_write) {
      fp_heap_hm_w = VG_(fopen)(dhat_hm_out_file_w, VKI_O_CREAT|VKI_O_TRUNC|VKI_O_WRONLY,
                        VKI_S_IRUSR|VKI_S_IWUSR);
      if (!fp_heap_hm_w) {
         VG_(umsg)("error: can't open DHAT heatmap output file '%s'\n", dhat_hm_out_file_w);
         return;
      }
   }
   if (clo_hm_read) {
      fp_heap_hm_r = VG_(fopen)(dhat_hm_out_file_r, VKI_O_CREAT|VKI_O_TRUNC|VKI_O_WRONLY,
                        VKI_S_IRUSR|VKI_S_IWUSR);
      if (!fp_heap_hm_r) {
         VG_(umsg)("error: can't open DHAT heatmap output file '%s'\n", dhat_hm_out_file_r);
         return;
      }
   }
   

   // Write to data file.
   FP("{\"dhatFileVersion\":1\n");

   // The command.
   const HChar* exe = VG_(args_the_exename);
   FP(",\"cmd\":\"%s", json_escape(exe));
   for (Word i = 0; i < VG_(sizeXA)(VG_(args_for_client)); i++) {
      const HChar* arg = *(HChar**)VG_(indexXA)(VG_(args_for_client), i);
      FP(" %s", json_escape(arg));
   }
   FP("\"\n");

   // The PID.
   FP(",\"pid\":%d\n", VG_(getpid)());

   // Times.
   FP(",\"mi\":%llu,\"ei\":%llu\n", g_max_instrs, g_curr_instrs);

   // APs.
   write_APInfos();

   // Heap heatmaps
   write_AP_HMs();

   // Frame table.
   FP(",\"ftbl\":\n");

   // The frame table maps strings to numbers. We want to print it ordered by
   // numbers. So we create an array and fill it in from the frame table, then
   // print that.
   UWord n_frames = next_frame_n;
   const HChar** frames =
      VG_(malloc)("dh.frames", n_frames * sizeof(const HChar*));
   VG_(initIterFM)(frame_tbl);
   while (VG_(nextIterFM)(frame_tbl, &keyW, &valW)) {
      const HChar* str = (const HChar*)keyW;
      UWord n = valW;
      frames[n] = str;
   }
   VG_(doneIterFM)(frame_tbl);

   for (UWord i = 0; i < n_frames; i++) {
      FP(" %c\"%s\"\n", i == 0 ? '[' : ',', json_escape(frames[i]));
   }
   FP(" ]\n");

   FP("}\n");

   VG_(fclose)(fp_heap);
   fp_heap = NULL;

   if (clo_hm_write) {
      VG_(fclose)(fp_heap_hm_w);
      fp_heap_hm_w = NULL;
   }
   if (clo_hm_read) {
      VG_(fclose)(fp_heap_hm_r);
      fp_heap_hm_r = NULL;
   }

   if (VG_(clo_verbosity) == 0) {
      return;
   }

   // Print brief global stats.
   VG_(umsg)("Total:     %'llu bytes in %'llu blocks\n",
             g_total_bytes, g_total_blocks);
   VG_(umsg)("At t-gmax: %'llu bytes in %'llu blocks\n",
             g_max_bytes, g_max_blocks);
   VG_(umsg)("At t-end:  %'llu bytes in %'llu blocks\n",
             g_curr_bytes, g_curr_blocks);
   VG_(umsg)("Reads:     %'llu bytes\n", g_reads_bytes);
   VG_(umsg)("Writes:    %'llu bytes\n", g_writes_bytes);

}


/*--------------------------------------------------------------------*/
/*--- Discarding BB info                                           ---*/
/*--------------------------------------------------------------------*/

// Called when a translation is removed from the translation cache for
// any reason at all: to free up space, because the guest code was
// unmapped or modified, or for any arbitrary reason.
static
void cd_discard_superblock_info ( Addr orig_addr64, VexGuestExtents vge )
{
   SB_info* sbInfo;
   Addr     orig_addr = vge.base[0];

   tl_assert(vge.n_used > 0);

   if (DEBUG_CG)
      VG_(printf)( "discard_basic_block_info: %p, %p, %llu\n", 
                   (void*)orig_addr,
                   (void*)vge.base[0], (ULong)vge.len[0]);

   // Get BB info, remove from table, free BB info.  Simple!  Note that we
   // use orig_addr, not the first instruction address in vge.
   sbInfo = VG_(OSetGen_Remove)(instrInfoTable, &orig_addr);
   tl_assert(NULL != sbInfo);
   VG_(OSetGen_FreeNode)(instrInfoTable, sbInfo);
}

/*--------------------------------------------------------------------*/
/*--- Command line processing                                      ---*/
/*--------------------------------------------------------------------*/

static Bool cd_process_cmd_line_option(const HChar* arg)
{
   if (VG_(str_clo_cache_opt)(arg,
                              &clo_I1_cache,
                              &clo_D1_cache,
                              &clo_LL_cache)) {}

   else if VG_STR_CLO( arg, "--cachegrind-out-file", clo_cachegrind_out_file) {}
   else if VG_BOOL_CLO(arg, "--cache-sim",  clo_cache_sim)  {}
   else if VG_BOOL_CLO(arg, "--branch-sim", clo_branch_sim) {}
   else if VG_BOOL_CLO(arg, "--hm-read", clo_hm_read) {}
   else if VG_BOOL_CLO(arg, "--hm-write", clo_hm_write) {}
   else if VG_INT_CLO(arg, "--ts-res", clo_ts_res) {}
   else if VG_INT_CLO(arg, "--mem-res", clo_mem_res) {}
   else if VG_INT_CLO(arg, "--hm-size-limit", clo_hm_size_limit) {}
   else if VG_INT_CLO(arg, "--hm-writes-limit", clo_hm_writes_limit) {}
   else if VG_INT_CLO(arg, "--hm-reads-limit", clo_hm_reads_limit) {}
   else
      return False;

   return True;
}

static void cd_print_usage(void)
{
   VG_(print_cache_clo_opts)();
   VG_(printf)(
"    --cache-sim=yes|no               collect cache stats? [yes]\n"
"    --branch-sim=yes|no              collect branch prediction stats? [no]\n"
"    --cachegrind-out-file=<file>     output file name [cachegrind.out.%%p]\n"
   );
}

static void cd_print_debug_usage(void)
{
   VG_(printf)(
"    (none)\n"
   );
}

/*--------------------------------------------------------------------*/
/*--- Setup                                                        ---*/
/*--------------------------------------------------------------------*/

static void cd_post_clo_init(void); /* just below */

static void cd_pre_clo_init(void)
{
   VG_(details_name)            ("Cachegrind");
   VG_(details_version)         (NULL);
   VG_(details_description)     ("a cache and branch-prediction profiler");
   VG_(details_copyright_author)(
      "Copyright (C) 2002-2017, and GNU GPL'd, by Nicholas Nethercote et al.");
   VG_(details_bug_reports_to)  (VG_BUGS_TO);
   VG_(details_avg_translation_sizeB) ( 500 );

   VG_(clo_vex_control).iropt_register_updates_default
      = VG_(clo_px_file_backed)
      = VexRegUpdSpAtMemAccess; // overridable by the user.

   VG_(basic_tool_funcs)          (cd_post_clo_init,
                                   cd_instrument,
                                   cd_fini);

   VG_(needs_superblock_discards)(cd_discard_superblock_info);
   VG_(needs_command_line_options)(cd_process_cmd_line_option,
                                   cd_print_usage,
                                   cd_print_debug_usage);

   // malloc profile
   VG_(needs_libc_freeres)();
   VG_(needs_cxx_freeres)();
   VG_(umsg)("add malloc_replacement module\n");
   VG_(needs_malloc_replacement)  (dh_malloc,
                                   dh___builtin_new,
                                   dh___builtin_vec_new,
                                   dh_memalign,
                                   dh_calloc,
                                   dh_free,
                                   dh___builtin_delete,
                                   dh___builtin_vec_delete,
                                   dh_realloc,
                                   dh_malloc_usable_size,
                                   0 );
   interval_tree = VG_(newFM)( VG_(malloc),
                               "dh.interval_tree.1",
                               VG_(free),
                               interval_tree_Cmp );

   apinfo = VG_(newFM)( VG_(malloc),
                        "dh.apinfo.1",
                        VG_(free),
                        NULL/*unboxedcmp*/ );
}

static void cd_post_clo_init(void)
{
   cache_t I1c, D1c, LLc; 

   CC_table =
      VG_(OSetGen_Create)(offsetof(LineCC, loc),
                          cmp_CodeLoc_LineCC,
                          VG_(malloc), "cd.main.cpci.1",
                          VG_(free));
   instrInfoTable =
      VG_(OSetGen_Create)(/*keyOff*/0,
                          NULL,
                          VG_(malloc), "cd.main.cpci.2",
                          VG_(free));
   stringTable =
      VG_(OSetGen_Create)(/*keyOff*/0,
                          stringCmp,
                          VG_(malloc), "cd.main.cpci.3",
                          VG_(free));

   VG_(post_clo_init_configure_caches)(&I1c, &D1c, &LLc,
                                       &clo_I1_cache,
                                       &clo_D1_cache,
                                       &clo_LL_cache);

   // min_line_size is used to make sure that we never feed
   // accesses to the simulator straddling more than two
   // cache lines at any cache level
   min_line_size = (I1c.line_size < D1c.line_size) ? I1c.line_size : D1c.line_size;
   min_line_size = (LLc.line_size < min_line_size) ? LLc.line_size : min_line_size;

   Int largest_load_or_store_size
      = VG_(machine_get_size_of_largest_guest_register)();
   if (min_line_size < largest_load_or_store_size) {
      /* We can't continue, because the cache simulation might
         straddle more than 2 lines, and it will assert.  So let's
         just stop before we start. */
      VG_(umsg)("Cachegrind: cannot continue: the minimum line size (%d)\n",
                (Int)min_line_size);
      VG_(umsg)("  must be equal to or larger than the maximum register size (%d)\n",
                largest_load_or_store_size );
      VG_(umsg)("  but it is not.  Exiting now.\n");
      VG_(exit)(1);
   }

   cachesim_initcaches(I1c, D1c, LLc);
}

VG_DETERMINE_INTERFACE_VERSION(cd_pre_clo_init)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/


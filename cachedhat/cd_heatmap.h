#include "pub_tool_wordfm.h"
#include "pub_tool_libcassert.h"
#include "stdlib.h"


#define HEATMAP_SIZE_LIMIT       1048576 // Better be LLC size
#define HEATMAP_READS_LIMIT      67108864
#define HEATMAP_WRITES_LIMIT     67108864
#define TSR_RES                  32768
#define TSW_RES                  16384
#define MEMR_RES                 32768
#define MEMW_RES                 16384

#define HEATMAPLIST_INIT_SIZE    16
#define TS_PRINT_THRS            16

struct HeatMapNode;
typedef 
   struct {
      struct HeatMapNode*      next;
      UInt*    mem_region;
      UInt     mem_region_size;
      UInt     ts_id;
   } HeatMapNode;

typedef
   struct {
      HeatMapNode*       HMHead;
      HeatMapNode*       HMNode;
   } HeatMapList;
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
      
      UInt                 ts_id_w;
      UInt                 ts_w;
      HeatMapList*         HMWList;
      UInt                 HMWListSize;
      UInt                 ts_id_r;
      UInt                 ts_r;
      HeatMapList*         HMRList;
      UInt                 HMRListSize
   }
   APInfo;

/* Tracks information about live blocks. */
typedef
   struct {
      Addr        payload;
      SizeT       req_szB;
      ExeContext* ap;  /* allocation ec */
      ULong       allocd_at; /* instruction number */
      ULong       reads_bytes;
      ULong       writes_bytes;
      /* Approx histogram, one byte per payload byte.  Counts latch up
         therefore at 0xFFFF.  Can be NULL if the block is resized or if
         the block is larger than HISTOGRAM_SIZE_LIMIT. */
      UShort*     histoW; /* [0 .. req_szB-1] */

      HeatMapList    HMR;
      HeatMapList    HMW;
   }
   Block;

// export function
extern UInt clo_tsr_res;
extern UInt clo_tsw_res;
extern UInt clo_memr_res;
extern UInt clo_memw_res;
extern Bool clo_hm_read;
extern Bool clo_hm_write;
extern Block* find_Block_containing ( Addr a );
extern void add_hm_node(HeatMapNode** node, UInt size, UInt ts_curr);

extern WordFM* apinfo ;  /* WordFM* ExeContext* APInfo* */

inline void heap_handle_write ( Addr addr, UWord szB )
{
   Block* bk = find_Block_containing(addr);
   if (bk) {
      APInfo* api   = NULL;
      UWord   keyW  = 0;
      UWord   valW  = 0;
      Bool    found = VG_(lookupFM)( apinfo,
                                    &keyW, &valW, (UWord)bk->ap );
      tl_assert(found);
      api = (APInfo*)valW;
      bk->writes_bytes += szB;
      if(clo_hm_write && bk->HMW.HMHead != NULL) {
         if (bk->HMW.HMNode->ts_id != api->ts_id_w) {
            add_hm_node(&(bk->HMW.HMNode), bk->HMW.HMNode->mem_region_size, api->ts_id_w);
         }
         bk->HMW.HMNode->mem_region[(addr-bk->payload)/clo_memw_res]++;
         if(++(api->ts_w) == clo_tsw_res) {
            api->ts_w = 0;
            api->ts_id_w++;
            add_hm_node(&(bk->HMW.HMNode), bk->HMW.HMNode->mem_region_size, api->ts_id_w);
         }
      }
   }
}

inline void heap_handle_read ( Addr addr, UWord szB )
{
   Block* bk = find_Block_containing(addr);
   if (bk) {
      APInfo* api   = NULL;
      UWord   keyW  = 0;
      UWord   valW  = 0;
      Bool    found = VG_(lookupFM)( apinfo,
                                    &keyW, &valW, (UWord)bk->ap );
      tl_assert(found);
      api = (APInfo*)valW;
      bk->reads_bytes += szB;
      if(clo_hm_read && bk->HMR.HMHead != NULL) {
         if (bk->HMR.HMNode->ts_id != api->ts_id_r) {
            add_hm_node(&(bk->HMR.HMNode), bk->HMR.HMNode->mem_region_size, api->ts_id_r);
         }
         bk->HMR.HMNode->mem_region[(addr-bk->payload)/clo_memr_res]++;
         if(++(api->ts_r) == clo_tsr_res) {
            api->ts_r = 0;
            api->ts_id_r++;
            add_hm_node(&(bk->HMR.HMNode), bk->HMR.HMNode->mem_region_size, api->ts_id_r);
         }
      }
   }
}

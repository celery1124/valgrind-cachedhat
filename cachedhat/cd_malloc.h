


#define HISTOGRAM_SIZE_LOW 1048576 // Better be LLC size
#define TS_RES             16384
#define MEM_RES            16384

/* Tracks information about live blocks. */
struct Hist;
typedef 
   struct {
      struct Hist*    next;
      UInt*    mem_region;
      UInt     mem_region_size;
      UInt     ts;
   } Hist;
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
      Hist*       histHead;
      Hist*       histNode;
   }
   Block;

// export function
extern UInt clo_ts_res;
extern UInt clo_mem_res;
extern Block* find_Block_containing ( Addr a );
extern void add_hist_node(Hist** node, UInt size);

inline void malloc_handle_write ( Addr addr, UWord szB )
{
   Block* bk = find_Block_containing(addr);
   if (bk) {
      bk->writes_bytes += szB;
      if(bk->histHead) {
         bk->histNode->mem_region[(addr-bk->payload)/clo_mem_res]++;
         if(++(bk->histNode->ts) == clo_ts_res) add_hist_node(&(bk->histNode), bk->histNode->mem_region_size);
         //VG_(printf)("node %p, ts %d\n", bk->histNode, bk->histNode->ts);
      }
   }
}

inline void malloc_handle_read ( Addr addr, UWord szB )
{
   Block* bk = find_Block_containing(addr);
   if (bk) {
      bk->reads_bytes += szB;
      // if(bk->histHead) {
      //    bk->histNode->mem_region[(addr-bk->payload)/clo_mem_res]++;
      //    if(++(bk->histNode->ts) == clo_ts_res) add_hist_node(&(bk->histNode));
      // }
   }
}

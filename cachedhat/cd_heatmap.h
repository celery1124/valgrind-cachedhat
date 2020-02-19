


#define HEAPMAP_SIZE_LIMIT 1048576 // Better be LLC size
#define TS_RES             16384
#define MEM_RES            16384

/* Tracks information about live blocks. */
struct HeatMap;
typedef 
   struct {
      struct HeatMap*    next;
      UInt*    mem_region_r;
      UInt*    mem_region_w;
      UInt     mem_region_size;
      UInt     ts_r;
      UInt     ts_w;
   } HeatMap;
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
      HeatMap*    HMHead;
      HeatMap*    HMNode;
   }
   Block;

// export function
extern UInt clo_ts_res;
extern UInt clo_mem_res;
extern Bool clo_hm_read;
extern Bool clo_hm_write;
extern Block* find_Block_containing ( Addr a );
extern void add_hm_node(HeatMap** node, UInt size);

inline void heap_handle_write ( Addr addr, UWord szB )
{
   Block* bk = find_Block_containing(addr);
   if (bk) {
      bk->writes_bytes += szB;
      if(clo_hm_write && bk->HMHead != NULL) {
         bk->HMNode->mem_region_w[(addr-bk->payload)/clo_mem_res]++;
         if(++(bk->HMNode->ts_w) == clo_ts_res) add_hm_node(&(bk->HMNode), bk->HMNode->mem_region_size);
         //VG_(printf)("node %p, ts_w %d\n", bk->HMNode, bk->HMNode->ts_w);
      }
   }
}

inline void heap_handle_read ( Addr addr, UWord szB )
{
   Block* bk = find_Block_containing(addr);
   if (bk) {
      bk->reads_bytes += szB;
      if(clo_hm_read && bk->HMHead != NULL) {
         bk->HMNode->mem_region_r[(addr-bk->payload)/clo_mem_res]++;
         if(++(bk->HMNode->ts_r) == clo_ts_res) add_hm_node(&(bk->HMNode), bk->HMNode->mem_region_size);
         //VG_(printf)("node %p, ts_r %d\n", bk->HMNode, bk->HMNode->ts_r);
      }
   }
}

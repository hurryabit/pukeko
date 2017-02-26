#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#define TAG_NIX 0
#define TAG_FWD 1
#define TAG_APP 2
#define TAG_INT 3
#define TAG_FUN 4
#define TAG_CON 5

#define TAG_MIN_GOOD TAG_APP
#define TAG_MAX_GOOD TAG_CON

typedef struct heap_cell {
  uint8_t tag;
  uint8_t extra_tag;
  uint8_t unused[6];
  struct heap_cell* dat1;
  struct heap_cell* dat2;
} heap_cell;

typedef heap_cell* stack_cell;

typedef struct {
  stack_cell* stack_start;
  stack_cell* base_ptr;
  stack_cell* stack_ptr;
  heap_cell*  cafs_start;
  heap_cell*  cafs_end;
  uint64_t    heap_size;
  heap_cell*  heap_start;
  heap_cell*  heap_middle;
  heap_cell*  heap_end;
  heap_cell*  heap_ptr;
  heap_cell*  heap_limit;
  uint64_t    num_allocs;
  uint64_t    num_gc_runs;
} gc_info;

void gc_init(gc_info* info) {
  uint64_t half_heap_cells = info->heap_size / 48;
  info->heap_size   = 48 * info->heap_size;
  info->heap_middle = info->heap_start  + half_heap_cells;
  info->heap_end    = info->heap_middle + half_heap_cells;
  info->heap_ptr    = info->heap_start;
  info->heap_limit  = info->heap_middle;
  info->num_allocs -= (uint64_t) info->heap_ptr;
}

void gc_print_cell(heap_cell* ptr) {
  uint64_t* raw_ptr = (uint64_t*) ptr;
  printf("<%llu(%u) %llu %llu> @ %llu",
         *raw_ptr, ptr->tag, *(raw_ptr+1), *(raw_ptr+2), (uint64_t) raw_ptr);
}

void gc_assert_good_tag(heap_cell* ptr, char* caller) {
  if (ptr->tag == TAG_NIX ||
      (TAG_MIN_GOOD <= ptr->tag && ptr->tag <= TAG_MAX_GOOD))
    return;

  printf("Found bad cell in %s: ", caller);
  gc_print_cell(ptr);
  printf("\n");
  exit(1);
}

// If ptr doesn't point into the heap, we return it unchanged.
// In particular, we return NULL unchanged.
heap_cell* gc_copy(gc_info* info, heap_cell* ptr) {
  if (ptr < info->heap_start || ptr >= info->heap_end)
    return ptr;

  if (ptr->tag == TAG_FWD)
    return ptr->dat1;

  gc_assert_good_tag(ptr, "gc_copy");

  heap_cell* copy_ptr = info->heap_ptr;
  info->heap_ptr += 1;
  /* printf("copy "); */
  /* gc_print_cell(ptr); */
  /* printf(" -> %lld\n", (uint64_t) heap_ptr); */

  *copy_ptr = *ptr;
  ptr->tag = TAG_FWD;
  ptr->extra_tag = 0;
  ptr->dat1 = copy_ptr;
  ptr->dat2 = NULL;
  return copy_ptr;
}

void gc_follow(gc_info* info, heap_cell*  ptr) {
  gc_assert_good_tag(ptr, "gc_follow");

  if (ptr->tag == TAG_APP || ptr->tag == TAG_CON) {
    ptr->dat1 = gc_copy(info, ptr->dat1);
    ptr->dat2 = gc_copy(info, ptr->dat2);
  }
}

void gc_stack_frame(gc_info* info, stack_cell* frame_top, stack_cell* frame_base) {
  for(stack_cell* frame_ptr = frame_top; frame_ptr <= frame_base; ++frame_ptr)
    *frame_ptr = gc_copy(info, *frame_ptr);
}

void gc_collect(gc_info* info, uint64_t heap_claim) {
  info->num_gc_runs += 1;
  info->num_allocs += (uint64_t) info->heap_ptr;

  if (info->heap_ptr <= info->heap_middle) {
    info->heap_ptr   = info->heap_middle;
    info->heap_limit = info->heap_end;
  }
  else {
    info->heap_ptr   = info->heap_start;
    info->heap_limit = info->heap_middle;
  }
  heap_cell* follow_ptr = info->heap_ptr;

  for (heap_cell* caf_ptr = info->cafs_start; caf_ptr < info->cafs_end; ++caf_ptr)
    gc_follow(info, caf_ptr);

  stack_cell* stack_ptr = info->stack_ptr;
  stack_cell* base_ptr  = info->base_ptr;
  gc_stack_frame(info, stack_ptr, base_ptr);

  while (base_ptr < info->stack_start) {
    stack_ptr = base_ptr + 3;
    base_ptr = (stack_cell*) *(base_ptr+1);
    gc_stack_frame(info, stack_ptr, base_ptr);
  }

  while (follow_ptr < info->heap_ptr) {
    gc_follow(info, follow_ptr);
    ++follow_ptr;
  }

  if (info->heap_ptr + heap_claim > info->heap_limit) {
    puts("HEAP EXHAUSTED");
    exit(1);
  }

  info->num_allocs -= (uint64_t) info->heap_ptr;
}

void gc_stats(gc_info* info, uint64_t steps, uint64_t checks) {
  info->num_allocs += (uint64_t) info->heap_ptr;

  printf("\n");
  printf("Steps  : %12llu\n", steps);
  printf("Allocs : %12llu          (Checks: %12llu)\n",
         info->num_allocs / 24, checks);
  printf("GC runs: %12llu\n", info->num_gc_runs);
}

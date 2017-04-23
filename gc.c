#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define TAG_NIX 0
#define TAG_FWD 1
#define TAG_APP 2
#define TAG_INT 3
#define TAG_FUN 4
#define TAG_CON 128

#define TAG_MIN_GOOD TAG_APP
#define TAG_MAX_GOOD TAG_FUN

#define LIM_CON 4

#define CAF_SIZE 3

typedef struct heap_cell {
  uint8_t tag;
  uint8_t arity;
  uint8_t size;
  uint8_t unused[5];
  struct heap_cell* data[0];
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
  uint64_t    alloc_bytes;
  uint64_t    num_gc_runs;
} gc_info;

heap_cell* from_start;
heap_cell* from_end;

void gc_init(gc_info* info) {
  uint64_t half_heap_cells = info->heap_size / 48;
  info->heap_size   = 48 * half_heap_cells;
  info->heap_middle = info->heap_start  + 3 * half_heap_cells;
  info->heap_end    = info->heap_middle + 3 * half_heap_cells;
  info->heap_ptr    = info->heap_start;
  info->heap_limit  = info->heap_middle;
  info->alloc_bytes -= (uint64_t) info->heap_ptr;
}

void gc_print_cell(heap_cell* ptr) {
  uint64_t* raw_ptr = (uint64_t*) ptr;
  printf("<0x%016llx(%u) 0x%016llx %016llx> @ 0x%016llx",
         *raw_ptr, ptr->tag, *(raw_ptr+1), *(raw_ptr+2), (uint64_t) raw_ptr);
}

void gc_assert_good_tag(heap_cell* ptr, char* caller, bool fwd_ok) {
  if (ptr->tag == TAG_NIX
      || (fwd_ok && ptr->tag == TAG_FWD)
      || (TAG_MIN_GOOD <= ptr->tag && ptr->tag <= TAG_MAX_GOOD)
      || (TAG_CON <= ptr->tag && ptr->tag < TAG_CON + LIM_CON))
    return;

  printf("Found bad cell in %s: ", caller);
  gc_print_cell(ptr);
  printf("\n");
  exit(1);
}

heap_cell* gc_copy(gc_info* info, heap_cell* ptr) {
  if (ptr == NULL) {
    fputs("NULL POINTER EXCEPTION", stderr);
    exit(1);
  }

  heap_cell* ptr0 = ptr;
  uint64_t fwd_chain = 0;
  while (ptr->tag == TAG_FWD) {
    ptr = ptr->data[0];
  }
  gc_assert_good_tag(ptr, "gc_copy", false);

  if (from_start <= ptr && ptr < from_end) {
    heap_cell* copy_ptr = info->heap_ptr;
    info->heap_ptr += ptr->size;

    // TODO: A handwritten loop might be faster since everything is aligned
    // neatly.
    memcpy(copy_ptr, ptr, 8*ptr->size);
    ptr->tag = TAG_FWD;
    ptr->arity = 0;
    ptr->data[0] = copy_ptr;
    ptr = copy_ptr;
  }

  while (ptr0->tag == TAG_FWD) {
    heap_cell* ptr1 = ptr0->data[0];
    ptr0->data[0] = ptr;
    ptr0 = ptr1;
  }

  return ptr;
}

void gc_follow(gc_info* info, heap_cell*  ptr) {
  if (ptr->tag == TAG_FWD) {
    ptr->data[0] = gc_copy(info, ptr->data[0]);
  }

  if (ptr->tag == TAG_APP) {
    ptr->data[0] = gc_copy(info, ptr->data[0]);
    ptr->data[1] = gc_copy(info, ptr->data[1]);
  }

  if (ptr->tag >= TAG_CON) {
    for (uint8_t i = 0; i < ptr->arity; ++i) {
      ptr->data[i] = gc_copy(info, ptr->data[i]);
    }
  }
}

void gc_stack_frame(gc_info* info, stack_cell* frame_top, stack_cell* frame_base) {
  for(stack_cell* frame_ptr = frame_top; frame_ptr <= frame_base; ++frame_ptr)
    *frame_ptr = gc_copy(info, *frame_ptr);
}

void gc_collect(gc_info* info, uint64_t heap_claim) {
  /* puts("entering gc"); */
  if (info->heap_ptr > info->heap_limit) {
    fputs("OVERFILLED HEAP", stderr);
    exit(1);
  }
  info->num_gc_runs += 1;
  info->alloc_bytes += (uint64_t) info->heap_ptr;

  if (info->heap_ptr <= info->heap_middle) {
    info->heap_ptr   = info->heap_middle;
    info->heap_limit = info->heap_end;
    from_start = info->heap_start;
    from_end   = info->heap_middle;
  }
  else {
    info->heap_ptr   = info->heap_start;
    info->heap_limit = info->heap_middle;
    from_start = info->heap_middle;
    from_end   = info->heap_end;
  }
  heap_cell* follow_ptr = info->heap_ptr;

  for (heap_cell* caf_ptr = info->cafs_start; caf_ptr < info->cafs_end; caf_ptr += CAF_SIZE) {
    gc_assert_good_tag(caf_ptr, "gc_follow@cafs", true);
    gc_follow(info, caf_ptr);
  }

  stack_cell* stack_ptr = info->stack_ptr;
  stack_cell* base_ptr  = info->base_ptr;
  gc_stack_frame(info, stack_ptr, base_ptr);

  while (base_ptr < info->stack_start) {
    stack_ptr = base_ptr + 3;
    base_ptr = (stack_cell*) *(base_ptr+1);
    gc_stack_frame(info, stack_ptr, base_ptr);
  }

  while (follow_ptr < info->heap_ptr) {
    gc_assert_good_tag(follow_ptr, "gc_follow@finish", false);
    gc_follow(info, follow_ptr);
    follow_ptr += follow_ptr->size;
  }

  if (info->heap_ptr + heap_claim > info->heap_limit) {
    fputs("HEAP EXHAUSTED", stderr);
    exit(1);
  }

  info->alloc_bytes -= (uint64_t) info->heap_ptr;
  /* puts("leaving gc"); */
}

void gc_stats(gc_info* info, uint64_t steps, uint64_t checks) {
  info->alloc_bytes += (uint64_t) info->heap_ptr;

  fprintf(stderr, "Steps      : %12llu\n", steps);
  fprintf(stderr, "Alloc bytes: %12llu          (Checks: %12llu)\n",
         info->alloc_bytes, checks);
  fprintf(stderr, "GC runs    : %12llu\n", info->num_gc_runs);
}

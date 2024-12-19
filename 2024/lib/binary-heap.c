#include <assert.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include "binary-heap.h"
#include "types.h"

typedef struct BinaryHeap {
  void **items;
  size_t size;
  size_t length;
  CompareFn compare;
} BinaryHeap;

// Power of 2 so lowest parent nodes each have 2 children
static const size_t INITIAL_SIZE = 2048;

static size_t left_child(size_t parent) {
  return (parent * 2) + 1;
}

static size_t right_child(size_t parent) {
  return left_child(parent) + 1;
}

static size_t parent(size_t child) {
  return (child - 1) / 2;
}

static void swap_items(BinaryHeap *heap, size_t a_index, size_t b_index) {
  assert(heap != NULL && "heap is NULL");
  assert(a_index < heap->length && "invalid a_index");
  assert(b_index < heap->length && "invalid b_index");

  void *a = heap->items[a_index];
  heap->items[a_index] = heap->items[b_index];
  heap->items[b_index] = a;
}

BinaryHeap *binaryheap_new(CompareFn compare) {
  assert(compare != NULL && "compare function pointer is NULL");

  BinaryHeap *heap = malloc(sizeof(*heap));
  assert(heap != NULL && "malloc failed");

  heap->size = INITIAL_SIZE;
  heap->length = 0;
  heap->compare = compare;
  heap->items = malloc(sizeof(*heap->items) * heap->size);
  assert(heap->items != NULL && "malloc failed");

  return heap;
}

void binaryheap_free(BinaryHeap *heap) {
  assert(heap != NULL && "heap is NULL");

  free(heap->items);
  free(heap);
}

void binaryheap_foreach(BinaryHeap *heap,
                        void (*callback)(const void *item, size_t index)) {
  assert(heap != NULL && "heap is NULL");

  for (size_t i = 0; i < heap->length; i++) {
    void *item = heap->items[i];
    callback(item, i);
  }
}

bool binaryheap_is_empty(BinaryHeap *heap) {
  assert(heap != NULL && "heap is NULL");

  return heap->length == 0;
}

size_t binaryheap_length(BinaryHeap *heap) {
  assert(heap != NULL && "heap is NULL");

  return heap->length;
}

Option binaryheap_peek(BinaryHeap *heap) {
  assert(heap != NULL && "heap is NULL");

  if (heap->length == 0) {
    return none();
  }

  return some(heap->items[0]);
}

Option binaryheap_pop(BinaryHeap *heap) {
  assert(heap != NULL && "heap is NULL");

  if (heap->length == 0) {
    return none();
  }

  void *top = heap->items[0];

  if (heap->length == 1) {
    heap->length = 0;
    return some(top);
  }

  void *bottom = heap->items[heap->length - 1];
  size_t bottom_index = 0;
  heap->items[bottom_index] = bottom;
  heap->length -= 1;

  size_t left_index = left_child(bottom_index);
  size_t right_index = right_child(bottom_index);

  while (left_index < heap->length) {
    void *left = heap->items[left_index];
    bool bottom_less_than_left = heap->compare(bottom, left) < 0;

    if (right_index >= heap->length) {
      if (bottom_less_than_left) {
        swap_items(heap, bottom_index, left_index);
        bottom_index = left_index;
      }
      break;
    }

    void *right = heap->items[right_index];
    bool bottom_less_than_right = heap->compare(bottom, right) < 0;

    if (!bottom_less_than_left && !bottom_less_than_right) {
      break;
    }

    bool less_than_both_children =
        bottom_less_than_left && bottom_less_than_right;
    bool left_less_than_right = heap->compare(left, right) < 0;
    bool less_than_right_child_only =
        !bottom_less_than_left && bottom_less_than_right;

    size_t swap_index = left_index;
    if ((less_than_both_children && left_less_than_right) ||
        less_than_right_child_only) {
      swap_index = right_index;
    }

    swap_items(heap, bottom_index, swap_index);
    bottom_index = swap_index;

    left_index = left_child(bottom_index);
    right_index = right_child(bottom_index);
  }

  return some(top);
}

void binaryheap_push(BinaryHeap *heap, void *item) {
  assert(heap != NULL && "heap is NULL");

  if (heap->length == heap->size) {
    heap->size *= 2;
    heap->items = realloc(heap->items, sizeof(*heap->items) * heap->size);
    assert(heap->items != NULL && "realloc failed");
  }

  size_t item_index = heap->length;
  heap->items[item_index] = item;
  heap->length += 1;

  while (item_index > 0) {
    size_t parent_index = parent(item_index);
    void *parent = heap->items[parent_index];

    if (heap->compare(parent, item) >= 0) {
      break;
    }

    swap_items(heap, parent_index, item_index);
    item_index = parent_index;
  }
}

void binaryheap_clear(BinaryHeap *heap) {
  assert(heap != NULL && "heap is NULL");

  heap->items = realloc(heap->items, sizeof(*heap->items) * INITIAL_SIZE);
  assert(heap->items != NULL && "realloc failed");

  heap->length = 0;
}

#ifndef BINARY_HEAP_H
#define BINARY_HEAP_H

#include <stdbool.h>
#include <stddef.h>

#include "types.h"

typedef struct BinaryHeap BinaryHeap;

BinaryHeap *binaryheap_new(CompareFn compare);
void binaryheap_free(BinaryHeap *heap);

void binaryheap_foreach(BinaryHeap *heap,
                        void (*callback)(const void *item, size_t index));

bool binaryheap_is_empty(BinaryHeap *heap);
size_t binaryheap_length(BinaryHeap *heap);

Option binaryheap_peek(BinaryHeap *heap);
Option binaryheap_pop(BinaryHeap *heap);
void binaryheap_push(BinaryHeap *heap, void *item);
void binaryheap_clear(BinaryHeap *heap);

#endif

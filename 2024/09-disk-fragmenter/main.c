/*
Day 9: Disk Fragmenter
https://adventofcode.com/2024/day/9
*/

// #define USE_EXAMPLE
#define _DEFAULT_SOURCE

#include <assert.h>
#include <ctype.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../lib/aoc.h"
#include "../lib/txt.h"
#include "main.h"

typedef enum DiskItemType {
  DISK_ITEM_TYPE_FILE,
  DISK_ITEM_TYPE_FREESPACE,
} DiskItemType;

typedef struct DiskItem {
  DiskItemType type;
  uint64_t id;
  uint8_t block_size;
} DiskItem;

typedef struct DiskMap {
  DiskItem *items;
  size_t length;
} DiskMap;

typedef struct DiskBlock {
  DiskItemType type;
  uint64_t id;
} DiskBlock;

typedef struct DiskLayout {
  DiskBlock *blocks;
  size_t length;
} DiskLayout;

void disk_map_free(DiskMap *map) {
  free(map->items);
  free(map);
}

void disk_map_print(DiskMap *map) {
  printf("Disk Map\n");
  for (size_t i = 0; i < map->length; i++) {
    DiskItem item = map->items[i];
    for (uint8_t block = 0; block < item.block_size; block++) {
      if (item.type == DISK_ITEM_TYPE_FILE) {
        printf("%" PRId64, item.id);
      } else {
        printf(".");
      }
    }
  }
  printf("\n");
}

DiskMap *disk_map_parse(char *map_str) {
  DiskMap *map = malloc(sizeof(*map));
  assert(map != NULL);

  map->length = strlen(map_str);
  map->items = malloc(sizeof(*map->items) * map->length);
  assert(map->items != NULL && "malloc failed");

  uint64_t id = 0;
  size_t num_items = 0;
  bool is_file = true;
  char curr_digit;

  while ((curr_digit = *map_str++)) {
    assert(isdigit(curr_digit) && "invalid block size");

    map->items[num_items++] = (DiskItem){
        .type = is_file ? DISK_ITEM_TYPE_FILE : DISK_ITEM_TYPE_FREESPACE,
        .id = id,
        .block_size = (uint8_t)curr_digit - '0'};

    if (is_file) {
      id++;
    }

    is_file = !is_file;
  }
  assert(num_items == map->length && "invalid disk item count");

  return map;
}

void disk_layout_free(DiskLayout *layout) {
  free(layout->blocks);
  free(layout);
}

void disk_layout_print(DiskLayout *layout) {
  printf("Disk Layout\n");
  for (size_t i = 0; i < layout->length; i++) {
    DiskBlock block = layout->blocks[i];
    if (block.type == DISK_ITEM_TYPE_FILE) {
      printf("%" PRId64, block.id);
    } else {
      printf(".");
    }
  }
  printf("\n");
}

size_t disk_layout_length(DiskMap *map) {
  size_t length = 0;
  for (size_t i = 0; i < map->length; i++) {
    length += map->items[i].block_size;
  }
  return length;
}

DiskLayout *disk_layout_parse(DiskMap *map) {
  DiskLayout *layout = malloc(sizeof(*layout));
  assert(layout != NULL && "malloc failed");

  layout->length = disk_layout_length(map);
  layout->blocks = malloc(sizeof(*layout->blocks) * layout->length);
  assert(layout->blocks != NULL && "malloc failed");

  size_t num_blocks = 0;
  for (size_t i = 0; i < map->length; i++) {
    DiskItem item = map->items[i];
    for (size_t block = 0; block < item.block_size; block++) {
      layout->blocks[num_blocks++] =
          (DiskBlock){.type = item.type, .id = item.id};
    }
  }
  assert(layout->length == num_blocks && "invalid block count");

  return layout;
}

size_t next_free_block(DiskBlock **nextp, DiskBlock *blocks,
                       size_t num_blocks) {
  for (size_t i = 0; i < num_blocks - 1; i++) {
    DiskBlock *next = blocks + i;
    if (next->type == DISK_ITEM_TYPE_FREESPACE) {
      *nextp = next;
      return i;
    }
  }

  nextp = NULL;
  return 0;
}

size_t next_file_block(DiskBlock **nextp, DiskBlock *blocks,
                       size_t num_blocks) {
  for (size_t i = num_blocks - 1; i > 0; i--) {
    DiskBlock *next = blocks + i;
    if (next->type == DISK_ITEM_TYPE_FILE) {
      *nextp = next;
      return i;
    }
  }

  nextp = NULL;
  return 0;
}

void compact_print_state(DiskLayout *layout, size_t free_offset,
                         size_t file_offset) {
  disk_layout_print(layout);
  for (size_t i = 0; i < layout->length; i++) {
    if (i == free_offset) {
      printf("L");
    } else if (i == file_offset) {
      printf("R");
    } else {
      printf(" ");
    }
  }
  printf("\nfree_block_offset (L): %zu\n", free_offset);
  printf("file_block: id=%" PRId64 "\n", layout->blocks[file_offset].id);
  printf("file_block_offset (R): %zu\n", file_offset);
  printf("\n");
}

void disk_layout_compact(DiskLayout *layout) {
  DiskBlock *free_block;
  DiskBlock *file_block;

  const DiskBlock FREE_BLOCK = {.type = DISK_ITEM_TYPE_FREESPACE};

  size_t free_block_offset =
      next_free_block(&free_block, layout->blocks, layout->length);
  size_t file_block_offset =
      next_file_block(&file_block, layout->blocks, layout->length);

  while (free_block != NULL && file_block != NULL && file_block > free_block) {

#ifdef USE_EXAMPLE
    printf("Compact...\n");
    compact_print_state(layout, free_block_offset, file_block_offset);
#endif

    layout->blocks[free_block_offset] = *file_block;
    layout->blocks[file_block_offset] = FREE_BLOCK;

#ifdef USE_EXAMPLE
    printf("Moving file block...\n");
    compact_print_state(layout, free_block_offset, file_block_offset);
#endif

    free_block_offset++;
    free_block_offset +=
        next_free_block(&free_block, layout->blocks + free_block_offset,
                        layout->length - free_block_offset);

    file_block_offset =
        next_file_block(&file_block, layout->blocks, file_block_offset);
  }

  return;
}

uint64_t disk_layout_checksum(DiskLayout *layout) {
  uint64_t checksum = 0;
  for (size_t i = 0; i < layout->length; i++) {
    DiskBlock block = layout->blocks[i];
    if (block.type == DISK_ITEM_TYPE_FILE) {
      checksum += i * block.id;
    }
  }

  return checksum;
}

int main(void) {
  Txt *txt = txt_read(stdin);
  assert(txt->num_lines == 1 && "invalid puzzle input");

  DiskMap *map = disk_map_parse(txt->lines[0]);
  DiskLayout *layout = disk_layout_parse(map);

  disk_layout_compact(layout);
  uint64_t layout_checksum = disk_layout_checksum(layout);

  disk_layout_free(layout);
  disk_map_free(map);
  txt_free(txt);

  print_day(9, "Disk Fragmenter");
  printf("Part 1: %" PRIu64 " \n", layout_checksum);
  printf("Part 2: TODO\n");

  assert(layout_checksum == PART1_ANSWER);
  // assert(0 == PART2_ANSWER);

  return 0;
}

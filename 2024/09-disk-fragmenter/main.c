/*
Day 9: Disk Fragmenter
https://adventofcode.com/2024/day/9
*/

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

// #define USE_EXAMPLE

#ifdef USE_EXAMPLE
#define PART1_ANSWER 1928
#define PART2_ANSWER 2858
#else
#define PART1_ANSWER 6310675819476
#define PART2_ANSWER 6335972980679
#endif

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

void disk_layout_compact(DiskLayout *layout) {
  DiskBlock *free_block;
  DiskBlock *file_block;

  const DiskBlock FREE_BLOCK = {.type = DISK_ITEM_TYPE_FREESPACE};

  size_t free_block_offset =
      next_free_block(&free_block, layout->blocks, layout->length);
  size_t file_block_offset =
      next_file_block(&file_block, layout->blocks, layout->length);

  while (free_block != NULL && file_block != NULL && file_block > free_block) {
    layout->blocks[free_block_offset] = *file_block;
    layout->blocks[file_block_offset] = FREE_BLOCK;

    free_block_offset++;
    free_block_offset +=
        next_free_block(&free_block, layout->blocks + free_block_offset,
                        layout->length - free_block_offset);

    file_block_offset =
        next_file_block(&file_block, layout->blocks, file_block_offset);
  }

  return;
}

void disk_layout_defrag(DiskLayout *layout, DiskMap *map) {
  for (size_t i = map->length - 1; i > 0; i--) {
    DiskItem item = map->items[i];
    if (item.type == DISK_ITEM_TYPE_FREESPACE) {
      continue;
    }

    for (size_t j = 0; j < i; j++) {
      DiskItem *swap_item = &map->items[j];
      bool swap_is_file = swap_item->type == DISK_ITEM_TYPE_FILE;
      bool swap_is_too_small = swap_item->block_size < item.block_size;

      if (swap_is_file || swap_is_too_small) {
        continue;
      }

      DiskBlock *old_file_blocks = layout->blocks;
      while (old_file_blocks->type != DISK_ITEM_TYPE_FILE ||
             old_file_blocks->id != item.id) {
        old_file_blocks++;
      }

      for (size_t k = 0; k < item.block_size; k++) {
        old_file_blocks[k].type = DISK_ITEM_TYPE_FREESPACE;
        // The IDs of these new FREESPACE blocks now have no meaning
      }

      DiskBlock *free_blocks = layout->blocks;
      while (free_blocks->type != DISK_ITEM_TYPE_FREESPACE ||
             free_blocks->id != swap_item->id) {
        // Luckily disk_map_parse assigns an ID to each FREESPACE disk item
        // It's the ID of the previous FILE seen, so is unique per FREESPACE
        free_blocks++;
      }

      for (size_t k = 0; k < item.block_size; k++) {
        free_blocks[k].type = DISK_ITEM_TYPE_FILE;
        free_blocks[k].id = item.id;
      }

      // Mangles the map, but it isn't used again after this function is done
      swap_item->block_size -= item.block_size;
      break;
    }
  }
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

  DiskLayout *compact_layout = disk_layout_parse(map);
  disk_layout_compact(compact_layout);
  uint64_t compact_layout_checksum = disk_layout_checksum(compact_layout);

  DiskLayout *defragged_layout = disk_layout_parse(map);
  disk_layout_defrag(defragged_layout, map);
  uint64_t defragged_layout_checksum = disk_layout_checksum(defragged_layout);

  txt_free(txt);
  disk_map_free(map);
  disk_layout_free(compact_layout);
  disk_layout_free(defragged_layout);

  print_day(9, "Disk Fragmenter");
  print_part_uint64(1, compact_layout_checksum, PART1_ANSWER);
  print_part_uint64(2, defragged_layout_checksum, PART2_ANSWER);

  return 0;
}

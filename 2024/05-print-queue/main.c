/*
Day 5: Print Queue
https://adventofcode.com/2024/day/5
*/

// #define USE_EXAMPLE
#define _GNU_SOURCE // qsort_r

#include <assert.h>
#include <errno.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../lib/aoc.h"
#include "../lib/strutils.h"
#include "../lib/txt.h"
#include "main.h"

typedef struct OrderingRule {
  uint64_t page;
  uint64_t depends_on;
} OrderingRule;

typedef struct PageUpdate {
  uint64_t *pages;
  size_t length;
} PageUpdate;

typedef struct PrintQueue {
  OrderingRule *rules;
  size_t num_rules;

  PageUpdate *updates;
  size_t num_updates;

  PageUpdate *valid_updates;
  size_t num_valid_updates;

  PageUpdate *invalid_updates;
  size_t num_invalid_updates;
} PrintQueue;

void pq_free(PrintQueue *pq) {
  free(pq->rules);
  for (size_t i = 0; i < pq->num_updates; i++) {
    free(pq->updates[i].pages);
  }
  free(pq->updates);
  free(pq->valid_updates);
  free(pq->invalid_updates);
  free(pq);
}

PrintQueue *pq_parse(Txt *txt) {
  size_t num_rules = 0;
  while (**(txt->lines + num_rules)) {
    num_rules++;
  }
  size_t num_updates = txt->num_lines - num_rules - 1;

  OrderingRule *rules = malloc(sizeof(*rules) * num_rules);
  assert(rules != NULL && "malloc failed");
  PageUpdate *updates = malloc(sizeof(*updates) * num_updates);
  assert(updates != NULL && "malloc failed");

  for (size_t i = 0; i < num_rules; i++) {
    uint64_t depends_on, page;
    int matched =
        sscanf(txt->lines[i], "%" PRIu64 "|%" PRIu64, &depends_on, &page);
    assert(matched == 2 && "sscanf failed");
    rules[i] = (OrderingRule){.depends_on = depends_on, .page = page};
  }

  for (size_t i = 0; i < num_updates; i++) {
    char *update_str = strdup(*(txt->lines + (num_rules + 1) + i));
    assert(update_str != NULL && "strdup failed");

    uint64_t num_pages = str_cntocc(",", update_str) + 1;
    uint64_t *pages = malloc(sizeof(*pages) * num_pages);
    assert(pages != NULL && "malloc failed");
    PageUpdate update = {.pages = pages, .length = num_pages};

    char *str_to_free = update_str;
    char *token;
    size_t page_num = 0;
    // TODO: lib function for reading list of numbers? (also used in day2)
    while ((token = strsep(&update_str, ",")) != NULL) {
      errno = 0;
      uint64_t page = strtoull(token, NULL, 10);
      assert(errno == 0 && "strtoull failed");
      update.pages[page_num++] = page;
    }
    assert(page_num == num_pages && "invalid page count");
    free(str_to_free);

    updates[i] = update;
  }

  PrintQueue *pq = malloc(sizeof(PrintQueue));
  assert(pq != NULL && "malloc failed");

  pq->rules = rules;
  pq->num_rules = num_rules;
  pq->updates = updates;
  pq->num_updates = num_updates;
  pq->valid_updates = NULL;
  pq->num_valid_updates = 0;
  pq->invalid_updates = NULL;
  pq->num_invalid_updates = 0;

  return pq;
}

bool pq_has_rule(PrintQueue *pq, uint64_t depends_on, uint64_t page) {
  for (size_t i = 0; i < pq->num_rules; i++) {
    OrderingRule rule = pq->rules[i];
    if (rule.page == page && rule.depends_on == depends_on) {
      return true;
    }
  }
  return false;
}

bool is_valid_update(PageUpdate *update, PrintQueue *pq) {
  for (size_t i = 0; i < update->length - 1; i++) {
    uint64_t page = update->pages[i];
    for (size_t j = i + 1; j < update->length; j++) {
      uint64_t depends_on = update->pages[j];
      if (pq_has_rule(pq, depends_on, page)) {
        // A page has been printed before one of it's dependencies
        return false;
      }
    }
  }
  return true;
}

void pq_validate(PrintQueue *pq) {
  PageUpdate *valid_updates = malloc(sizeof(*valid_updates) * pq->num_updates);
  assert(valid_updates != NULL && "malloc failed");
  PageUpdate *invalid_updates =
      malloc(sizeof(*invalid_updates) * pq->num_updates);
  assert(invalid_updates != NULL && "malloc failed");

  for (size_t i = 0; i < pq->num_updates; i++) {
    PageUpdate *update = pq->updates + i;

    if (is_valid_update(update, pq)) {
      valid_updates[pq->num_valid_updates++] = *update;
    } else {
      invalid_updates[pq->num_invalid_updates++] = *update;
    }
  }

  valid_updates =
      realloc(valid_updates, sizeof(*valid_updates) * pq->num_valid_updates);
  assert(valid_updates != NULL);
  invalid_updates = realloc(invalid_updates,
                            sizeof(*invalid_updates) * pq->num_invalid_updates);
  assert(invalid_updates != NULL);

  pq->valid_updates = valid_updates;
  pq->invalid_updates = invalid_updates;
}

// qsort_r has different argument orders depending on the platform 🙃
#ifdef __APPLE__
int compare_pages(void *print_queue, const void *left_page,
                  const void *right_page) {
#else
int compare_pages(const void *left_page, const void *right_page,
                  void *print_queue) {
#endif
  PrintQueue *pq = (PrintQueue *)print_queue;
  uint64_t left = *(uint64_t *)left_page;
  uint64_t right = *(uint64_t *)right_page;

  if (pq_has_rule(pq, right, left)) {
    // Left depends on right being printed first
    return -1;
  } else if (pq_has_rule(pq, left, right)) {
    // Right depends on left being printed first
    return 1;
  }
  return 0;
}

void pq_fix(PrintQueue *pq) {
  for (size_t i = 0; i < pq->num_invalid_updates; i++) {
    PageUpdate *update = pq->invalid_updates + i;

#ifdef __APPLE__ // Editor+LSP run on my Macbook, but I'm compiling and running
                 // in an Ubuntu container
    qsort_r(update->pages, update->length, sizeof(*update->pages), pq,
            compare_pages);
#else
    qsort_r(update->pages, update->length, sizeof(*update->pages),
            compare_pages, pq);
#endif
  }
}

uint64_t sum_middle_pages(PageUpdate *pu, size_t length) {
  uint64_t sum = 0;

  for (size_t i = 0; i < length; i++) {
    PageUpdate *update = pu + i;
    assert(update->length % 2 != 0 && "expected odd length (1 middle item)");
    sum += update->pages[(update->length / 2)];
  }

  return sum;
}

int main(void) {
  Txt *txt = txt_read(stdin);
  PrintQueue *pq = pq_parse(txt);
  pq_validate(pq);
  pq_fix(pq);

  uint64_t valid_sum =
      sum_middle_pages(pq->valid_updates, pq->num_valid_updates);
  uint64_t invalid_sum =
      sum_middle_pages(pq->invalid_updates, pq->num_invalid_updates);

  pq_free(pq);
  txt_free(txt);

  print_day(5, "Print Queue");
  printf("Part 1: %" PRIu64 "\n", valid_sum);
  printf("Part 2: %" PRIu64 "\n", invalid_sum);

  assert(valid_sum == PART1_ANSWER);
  assert(invalid_sum == PART2_ANSWER);

  return 0;
}

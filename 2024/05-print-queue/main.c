/*
Day 5: Print Queue
https://adventofcode.com/2024/day/5
*/

// #define USE_EXAMPLE
#define _DEFAULT_SOURCE

#include <assert.h>
#include <errno.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../lib/aoc.h"
#include "../lib/strutils.h"
#include "../lib/txt.h"
#include "main.h"

// TODO: move pq types and functions to separate file
// print-queue.c, print-queue.h
typedef struct OrderingRule {
  int page;
  int depends_on;
} OrderingRule;

typedef struct PageUpdate {
  int *pages;
  size_t length;
} PageUpdate;

typedef struct ValidPageUpdates {
  PageUpdate *updates;
  size_t length;
} ValidPageUpdates;

typedef struct PrintQueue {
  OrderingRule *rules;
  size_t num_rules;
  PageUpdate *updates;
  size_t num_updates;
} PrintQueue;

void valid_page_updates_free(ValidPageUpdates *vpu) {
  // Don't free each vpu->updates->pages, they are owned by pq
  free(vpu->updates);
  free(vpu);
}

void pq_free(PrintQueue *pq) {
  free(pq->rules);
  for (int i = 0; i < pq->num_updates; i++) {
    free(pq->updates[i].pages);
  }
  free(pq->updates);
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

  for (int i = 0; i < num_rules; i++) {
    int depends_on, page;
    int matched = sscanf(txt->lines[i], "%d|%d", &depends_on, &page);
    assert(matched == 2 && "sscanf failed");
    rules[i] = (OrderingRule){.depends_on = depends_on, .page = page};
  }

  for (int i = 0; i < num_updates; i++) {
    char *update_str = strdup(*(txt->lines + (num_rules + 1) + i));
    assert(update_str != NULL && "strdup failed");

    size_t num_pages = str_cntocc(",", update_str) + 1;
    int *pages = malloc(sizeof(*pages) * num_pages);
    assert(pages != NULL && "malloc failed");
    PageUpdate update = {.pages = pages, .length = num_pages};

    char *str_to_free = update_str;
    char *token;
    int page_num = 0;
    // TODO: lib function for reading list of numbers? (also used in day2)
    while ((token = strsep(&update_str, ",")) != NULL) {
      errno = 0;
      int page = strtol(token, NULL, 10);
      assert(errno == 0 && "strtol failed");
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

  return pq;
}

bool pq_has_rule(PrintQueue *pq, int page, int depends_on) {
  for (int i = 0; i < pq->num_rules; i++) {
    OrderingRule rule = pq->rules[i];
    if (rule.page == page && rule.depends_on == depends_on) {
      return true;
    }
  }
  return false;
}

bool is_valid_update(PageUpdate update, PrintQueue *pq) {
  for (int i = 0; i < update.length - 1; i++) {
    int page = update.pages[i];
    for (int j = i + 1; j < update.length; j++) {
      int depends_on = update.pages[j];
      if (pq_has_rule(pq, page, depends_on)) {
        return false;
      }
    }
  }
  return true;
}

ValidPageUpdates *pq_validate(PrintQueue *pq) {
  ValidPageUpdates *vpu = malloc(sizeof(*vpu));
  assert(vpu != NULL && "malloc failed");
  vpu->updates = malloc(sizeof(*vpu->updates) * pq->num_updates);
  assert(vpu->updates != NULL && "malloc failed");
  vpu->length = 0;

  for (int i = 0; i < pq->num_updates; i++) {
    PageUpdate update = pq->updates[i];

    if (is_valid_update(update, pq)) {
      vpu->updates[vpu->length++] = update;
    }
  }

  return vpu;
}

long sum_middle_pages(ValidPageUpdates *vpu) {
  long sum = 0;

  for (int i = 0; i < vpu->length; i++) {
    PageUpdate update = vpu->updates[i];
    assert(update.length % 2 != 0 && "expected odd length (1 middle item)");
    sum += update.pages[(update.length / 2)];
  }

  return sum;
}

int main(void) {
  Txt *txt = txt_read(stdin);
  PrintQueue *pq = pq_parse(txt);
  ValidPageUpdates *valid_updates = pq_validate(pq);

  long middle_page_sum = sum_middle_pages(valid_updates);

  valid_page_updates_free(valid_updates);
  pq_free(pq);
  txt_free(txt);

  print_day(5, "Print Queue");
  printf("Part 1: %ld\n", middle_page_sum);

  assert(middle_page_sum == PART1_ANSWER);
  // assert(0 == PART2_ANSWER);

  return 0;
}

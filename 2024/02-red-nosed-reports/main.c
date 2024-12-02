/*
Day 2: Red-Nosed Reports
https://adventofcode.com/2024/day/2
*/

#define _DEFAULT_SOURCE
#include <assert.h>
#include <errno.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// #define USE_EXAMPLE
#include "../lib/aoc.h"
#include "main.h"

int get_report(char **report) {
  *report = NULL;
  size_t report_len = 0;

  errno = 0;
  int result = getline(report, &report_len, stdin);
  assert(errno == 0 && "getline failed");

  if (result == -1) {
    *report = NULL;
    return -1;
  }

  (*report)[strcspn(*report, "\n")] = '\0';

  return report_len;
}

bool is_gradual_change(long prev_diff, long curr_diff) {
  if (curr_diff < -3 || curr_diff == 0 || curr_diff > 3) {
    return false; // Change is to big
  }

  if (prev_diff == 0) {
    return true; // There is no previous diff
  }

  bool is_decreasing = (prev_diff < 0 && curr_diff < 0);
  bool is_increasing = (prev_diff > 0 && curr_diff > 0);

  return is_decreasing || is_increasing;
}

int main(void) {
  char *report = NULL;

  long safe_reports = 0;

  while (get_report(&report) != -1) {
    char *report_to_free = report;
    char *token = strsep(&report, " ");
    assert(token != NULL);

    errno = 0;
    long prev_level = strtol(token, NULL, 10);
    assert(errno == 0 && "strtol failed");

    long prev_diff = 0;
    bool is_safe_report = true;

    while (is_safe_report && (token = strsep(&report, " ")) != NULL) {
      errno = 0;
      long curr_level = strtol(token, NULL, 10);
      assert(errno == 0 && "strtol failed");

      long curr_diff = curr_level - prev_level;

      is_safe_report = is_gradual_change(prev_diff, curr_diff);
      prev_level = curr_level;
      prev_diff = curr_diff;
    }

    if (is_safe_report) {
      safe_reports += 1;
    }

    free(report_to_free);
  }

  print_day(2, "Red-Nosed Reports");
  printf("Part 1: %ld\n", safe_reports);

  assert(safe_reports == PART1_ANSWER);
  // assert(0 == PART2_ANSWER);

  return 0;
}

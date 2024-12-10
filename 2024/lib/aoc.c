#include <inttypes.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include "ansi.h"
#include "aoc.h"

static void print_border(size_t length) {
  for (size_t i = 0; i < length; i++) {
    bool red = i % 2 == 0;
    ansi_esc(red ? ANSI_CODE_FG_RED : ANSI_CODE_FG_GREEN);
    putchar('-');
    ansi_reset();
  }
  putchar('\n');
}

void print_day(unsigned int day, const char *name) {
  char title[BUFSIZ];

  snprintf(title, BUFSIZ, "🎄 Day %d: ", day);

  size_t border_len = (strlen(title) + strlen(name)) * 2;

  print_border(border_len);

  ansi_esc(ANSI_CODE_FG_YELLOW);
  ansi_esc(ANSI_CODE_BOLD);
  printf("%s", title);
  ansi_reset();
  printf("%s\n", name);

  print_border(border_len);
}

void print_part(uint8_t part, uint64_t answer, uint64_t expected) {
  bool correct = answer == expected;

  ansi_esc(correct ? ANSI_CODE_FG_GREEN : ANSI_CODE_FG_RED);
  printf("Part %d: ", part);
  ansi_reset();

  printf("%" PRIu64, answer);

  ansi_esc(ANSI_CODE_BOLD);
  ansi_esc(correct ? ANSI_CODE_FG_GREEN : ANSI_CODE_FG_RED);
  printf("  ");
  printf(correct ? "✔️" : "x");
  ansi_reset();

  if (!correct) {
    ansi_esc(ANSI_CODE_FAINT);
    printf(" (should be %" PRIu64 ")", expected);
    ansi_reset();
  }

  printf("\n");
}

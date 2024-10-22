#include <stdio.h>
#include <string.h>

#include "aoc.h"

static void print_border(const unsigned int length) {
  for (int i = 0; i < length; i++) {
    putchar('-');
  }
  putchar('\n');
}

void print_day(const unsigned int day, const char *name) {
  char title[BUFSIZ];
  snprintf(title, BUFSIZ, "2024 Day %d: %s", day, name);

  size_t border_len = strlen(title) + 4;

  print_border(border_len);
  printf("%s\n", title);
  print_border(border_len);
}

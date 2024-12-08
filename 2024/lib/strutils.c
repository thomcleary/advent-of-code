#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "strutils.h"

char *str_rev(const char *str) {
  assert(str != NULL && "str is NULL");

  size_t len = strlen(str);
  char *reversed = malloc(len + 1);
  assert(reversed != NULL);

  for (size_t i = 0; i < len; i++) {
    reversed[i] = str[len - 1 - i];
  }
  reversed[len] = '\0';

  return reversed;
}

uint64_t str_cntocc(const char *word, const char *line) {
  size_t word_len = strlen(word);
  uint64_t count = 0;
  char *occurence = NULL;

  while ((occurence = strstr(line, word)) != NULL) {
    assert(count != UINT64_MAX && "too many occurences");
    count++;
    line = occurence + word_len;
  }

  return count;
}

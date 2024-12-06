#include <assert.h>
#include <stddef.h>
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

unsigned long str_cntocc(const char *word, const char *line) {
  size_t word_len = strlen(word);
  long count = 0;
  char *occurence = NULL;

  while ((occurence = strstr(line, word)) != NULL) {
    count++;
    assert(count >= 0 && "count overflowed");
    line = occurence + word_len;
  }

  return (unsigned long)count;
}

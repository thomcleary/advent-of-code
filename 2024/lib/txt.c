#define _DEFAULT_SOURCE

#include <assert.h>
#include <errno.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include "txt.h"

Txt *txt_new(size_t num_lines) {
  Txt *txt = malloc(sizeof(*txt));
  assert(txt != NULL && "malloc failed");

  txt->lines = malloc(sizeof(*txt->lines) * num_lines);
  assert(txt->lines != NULL && "malloc failed");

  txt->num_lines = num_lines;

  return txt;
}

Txt *txt_read(FILE *stream) {
  Txt *txt = txt_new(0);
  char *line = NULL;
  size_t buf_len = 0;

  errno = 0;
  while (getline(&line, &buf_len, stream) != -1) {
    txt->lines =
        realloc(txt->lines, sizeof(*txt->lines) * (txt->num_lines + 1));
    assert(txt->lines != NULL && "realloc failed");

    line[strcspn(line, "\n")] = '\0';
    txt->lines[txt->num_lines++] = strdup(line);
  }
  assert(errno == 0 && "getline failed");

  free(line);

  return txt;
}

void txt_free(Txt *txt) {
  for (int i = 0; i < txt->num_lines; i++) {
    free(txt->lines[i]);
  }
  free(txt->lines);
  free(txt);
}

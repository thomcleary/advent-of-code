#define _DEFAULT_SOURCE

#include <assert.h>
#include <errno.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "txt.h"

Txt *txt_new(size_t num_lines) {
  Txt *txt = malloc(sizeof(*txt));
  assert(txt != NULL && "malloc failed");

  txt->lines = malloc(sizeof(*txt->lines) * (size_t)num_lines);
  assert(txt->lines != NULL && "malloc failed");

  txt->num_lines = num_lines;

  return txt;
}

void txt_free(Txt *txt) {
  for (size_t i = 0; i < txt->num_lines; i++) {
    free(txt->lines[i]);
  }
  free(txt->lines);
  free(txt);
}

Txt *txt_read(FILE *stream) {
  Txt *txt = txt_new(0);
  char *line = NULL;
  size_t buf_len = 0;

  errno = 0;
  while (getline(&line, &buf_len, stream) != -1) {
    assert(txt->num_lines < SIZE_MAX && "too many lines");

    txt->lines =
        realloc(txt->lines, sizeof(*txt->lines) * ((size_t)txt->num_lines + 1));
    assert(txt->lines != NULL && "realloc failed");

    line[strcspn(line, "\n")] = '\0';
    txt->lines[txt->num_lines++] = strdup(line);
  }
  assert(errno == 0 && "getline failed");

  free(line);

  return txt;
}

Txt *txt_read_file(char *filename) {
  FILE *input_file = fopen(filename, "r");
  assert(input_file != NULL && "fopen failed");

  Txt *txt = txt_read(input_file);
  fclose(input_file);

  return txt;
}

void txt_print(Txt *txt) {
  for (size_t i = 0; i < txt->num_lines; i++) {
    printf("%s\n", txt->lines[i]);
  }
}

SplitTxt *txt_split(Txt *txt, char *sep) {
  SplitTxt *split_txt = malloc(sizeof(*split_txt));
  assert(split_txt != NULL && "malloc failed");

  split_txt->num_txts = 0;
  split_txt->txts = malloc(sizeof(*split_txt->txts) * txt->num_lines);
  assert(split_txt->txts != NULL && "malloc failed");

  char *line_buffer[txt->num_lines];
  size_t line_buffer_length = 0;

  for (size_t i = 0; i < txt->num_lines; i++) {
    char *line = txt->lines[i];

    if (strcmp(line, sep) == 0) {
      char **lines = malloc(sizeof(*lines) * line_buffer_length);
      assert(lines != NULL);
      memcpy(lines, line_buffer, (sizeof *line_buffer) * line_buffer_length);

      split_txt->txts[split_txt->num_txts++] =
          (Txt){.lines = lines, .num_lines = line_buffer_length};

      line_buffer_length = 0;
      continue;
    }

    char *line_dup = strdup(line);
    assert(line_dup != NULL && "strdup failed");
    line_buffer[line_buffer_length++] = line_dup;
  }

  if (line_buffer_length > 0) {
    char **lines = malloc(sizeof(*lines) * line_buffer_length);
    assert(lines != NULL);
    memcpy(lines, line_buffer, (sizeof *line_buffer) * line_buffer_length);

    split_txt->txts[split_txt->num_txts++] =
        (Txt){.lines = lines, .num_lines = line_buffer_length};
  }

  split_txt->txts =
      realloc(split_txt->txts, sizeof(*split_txt->txts) * split_txt->num_txts);
  assert(split_txt->txts != NULL && "realloc failed");

  return split_txt;
}

void split_txt_free(SplitTxt *split_txt) {
  for (size_t i = 0; i < split_txt->num_txts; i++) {
    Txt txt = split_txt->txts[i];
    for (size_t j = 0; j < txt.num_lines; j++) {
      free(txt.lines[j]);
    }
    free(txt.lines);
  }
  free(split_txt->txts);
  free(split_txt);
}

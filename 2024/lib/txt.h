#ifndef TXT_H
#define TXT_H

#include <stddef.h>
#include <stdio.h>

typedef struct Txt {
  char **lines;
  size_t num_lines;
} Txt;

Txt *txt_new(size_t num_lines);
Txt *txt_read(FILE *stream);
void txt_free(Txt *txt);

#endif

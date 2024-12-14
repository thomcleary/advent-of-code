#ifndef TXT_H
#define TXT_H

#include <stddef.h>
#include <stdio.h>

typedef struct {
  char **lines;
  size_t num_lines;
} Txt;

typedef struct {
  Txt *txts;
  size_t num_txts;
} SplitTxt;

Txt *txt_new(size_t num_lines);
void txt_free(Txt *txt);
Txt *txt_read(FILE *stream);
void txt_print(Txt *txt);

SplitTxt *txt_split(Txt *txt, char *sep);
void split_txt_free(SplitTxt *split_txt);

#endif

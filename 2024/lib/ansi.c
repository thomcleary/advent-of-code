#include <stdio.h>

#include "ansi.h"

void ansi_esc(AnsiCode code) {
  printf("\033[%dm", code);
}

void ansi_reset(void) {
  printf("\033[0m");
}

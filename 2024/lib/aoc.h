#ifndef AOC_H
#define AOC_H

#include <stdint.h>

void print_day(const unsigned int day, const char *name);

void print_part_uint64(uint8_t part, uint64_t answer, uint64_t expected);
void print_part_str(uint8_t part, char *answer, char *expected);

#endif

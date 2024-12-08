#ifndef STRUTILS_H
#define STRUTILS_H

#include <stdint.h>

/*
Returns a pointer to a new string which is the reverse of `str`.
Can be freed with with free(3)
*/
char *str_rev(const char *str);

/*
Returns the count of occurences of `word` in `line`
*/
uint64_t str_cntocc(const char *word, const char *line);

#endif

#ifndef PERMUTATION_H
#define PERMUTATION_H

#include <stdbool.h>
#include <stddef.h>

/**
 * see: https://en.wikipedia.org/wiki/Permutation#Generation_in_lexicographic_order
 */
bool next_permutation(char **strings, size_t num_strings);

#endif
#ifndef HASHTABLE_H
#define HASHTABLE_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#include "types.h"

typedef struct Hashtable Hashtable;

Hashtable *hashtable_new(void);
void hashtable_free(Hashtable *ht);

size_t hashtable_size(Hashtable *ht);
bool hashtable_has(Hashtable *ht, const char *key);

Option hashtable_get(Hashtable *ht, const char *key);
Hashtable *hashtable_set(Hashtable *ht, const char *key, const void *value);

#endif

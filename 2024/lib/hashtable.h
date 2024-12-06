#ifndef HASHTABLE_H
#define HASHTABLE_H

#include <stdbool.h>
#include <stddef.h>

typedef struct Hashtable Hashtable;

Hashtable *hashtable_new(void);
void hashtable_free(Hashtable *ht);
bool hashtable_has(Hashtable *ht, const char *key);
void *hashtable_get(Hashtable *ht, const char *key);
Hashtable *hashtable_set(Hashtable *ht, const char *key, const void *value);
size_t hashtable_size(Hashtable *ht);

#endif

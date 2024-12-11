#ifndef HASHTABLE_H
#define HASHTABLE_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

typedef struct Hashtable Hashtable;

typedef struct HashtableGetResult {
  bool success;
  void *value;
} HashtableGetResult;

Hashtable *hashtable_new(void);
void hashtable_free(Hashtable *ht);
bool hashtable_has(Hashtable *ht, const char *key);
HashtableGetResult hashtable_get(Hashtable *ht, const char *key);
Hashtable *hashtable_set(Hashtable *ht, const char *key, const void *value);
size_t hashtable_size(Hashtable *ht);

#endif

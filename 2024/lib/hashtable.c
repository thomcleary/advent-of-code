/*
Inspired by: https://github.com/benhoyt/ht
(benhoyt/ht shows how to implement an open-addressed hashtable)

I'll implement a separate-chained hashtable so I don't just copy and paste lol.
*/

#define _DEFAULT_SOURCE

#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "hashtable.h"

// This is the lowest power of 2 that causes no collisions with my puzzle input
#define NUM_BUCKETS 2048

// Carefully chosen by people with PhDs
#define FNV_OFFSET 14695981039346656037UL
#define FNV_PRIME 1099511628211UL

typedef struct HashtableEntry {
  const char *key;
  void *value;
  struct HashtableEntry *next;
} HashtableEntry;

typedef struct Hashtable {
  HashtableEntry *buckets[NUM_BUCKETS];
  size_t size;
} Hashtable;

static void free_entry(HashtableEntry *entry) {
  if (entry == NULL) {
    return;
  }

  free_entry(entry->next);
  free((char *)entry->key);
  free(entry);
}

/**
 * Returns 64-bit FNV-1a hash for key (NUL-terminated).
 * See description: https://en.wikipedia.org/wiki/Fowler–Noll–Vo_hash_function
 */
static uint64_t hash_key(const char *key) {
  assert(key != NULL);

  uint64_t hash = FNV_OFFSET;
  for (const char *p = key; *p; p++) {
    hash ^= (uint64_t)(unsigned char)(*p);
    hash *= FNV_PRIME;
  }
  return hash;
}

Hashtable *hashtable_new(void) {
  Hashtable *ht = malloc(sizeof(*ht));
  assert(ht != NULL);

  for (int i = 0; i < NUM_BUCKETS; i++) {
    ht->buckets[i] = NULL;
  }

  ht->size = 0;

  return ht;
}

void hashtable_free(Hashtable *ht) {
  if (ht == NULL) {
    return;
  }

  for (unsigned int bucket = 0; bucket < NUM_BUCKETS; bucket++) {
    HashtableEntry *entry = ht->buckets[bucket];
    free_entry(entry);
  }

  free(ht);
}

bool hashtable_has(Hashtable *ht, const char *key) {
  assert(ht != NULL);
  assert(key != NULL);

  HashtableEntry *entry = ht->buckets[hash_key(key) % NUM_BUCKETS];

  while (entry != NULL && strcmp(entry->key, key) != 0) {
    entry = entry->next;
  }

  return entry != NULL;
}

HashtableGetResult hashtable_get(Hashtable *ht, const char *key) {
  assert(ht != NULL);
  assert(key != NULL);

  HashtableEntry *entry = ht->buckets[hash_key(key) % NUM_BUCKETS];

  while (entry != NULL && strcmp(entry->key, key) != 0) {
    entry = entry->next;
  }

  if (entry == NULL) {
    return (HashtableGetResult){.success = false};
  }

  return (HashtableGetResult){.success = true, .value = entry->value};
}

Hashtable *hashtable_set(Hashtable *ht, const char *key, const void *value) {
  assert(ht != NULL);
  assert(key != NULL);

  uint64_t bucket = hash_key(key) % NUM_BUCKETS;
  HashtableEntry *prev_entry = ht->buckets[bucket];
  HashtableEntry *curr_entry = prev_entry;

  while (curr_entry != NULL) {
    if (strcmp(curr_entry->key, key) == 0) {
      curr_entry->value = (void *)value;
      return ht;
    }

    prev_entry = curr_entry;
    curr_entry = curr_entry->next;
  }

  HashtableEntry *new_entry = malloc(sizeof(*new_entry));
  assert(new_entry != NULL);

  new_entry->key = strdup(key);
  assert(new_entry->key != NULL);

  new_entry->value = (void *)value;
  new_entry->next = NULL;

  if (prev_entry == NULL) {
    ht->buckets[bucket] = new_entry;
  } else {
    prev_entry->next = new_entry;
  }

  ht->size++;
  return ht;
}

size_t hashtable_size(Hashtable *ht) {
  return ht->size;
}

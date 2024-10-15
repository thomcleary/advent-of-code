/*
Inspired by: https://github.com/benhoyt/ht
(benhoyt/ht shows how to implement an open-addressed hashtable)

I'll implement a separate-chained hashtable so I don't just copy and paste lol.
*/

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#include "hashtable.h"

// This is the lowest power of 2 that causes no collisions with my puzzle input
#define NUM_BUCKETS 2048

// Carefully chosen by people with PhDs
#define FNV_OFFSET 14695981039346656037UL
#define FNV_PRIME 1099511628211UL

struct hashtable_entry
{
    const char *key;
    void *value;
    struct hashtable_entry *next;
};

struct hashtable
{
    struct hashtable_entry *buckets[NUM_BUCKETS];
    size_t size;
};

static void free_entry(struct hashtable_entry *entry)
{
    if (entry == NULL)
    {
        return;
    }

    if (entry->next != NULL)
    {
        free(entry->next);
        free(entry->next);
    }

    free((char *)entry->key);
    free(entry);
}

/**
 * Returns 64-bit FNV-1a hash for key (NUL-terminated).
 * See description: https://en.wikipedia.org/wiki/Fowler–Noll–Vo_hash_function
 */
static uint64_t hash_key(const char *key)
{
    assert(key != NULL);

    uint64_t hash = FNV_OFFSET;
    for (const char *p = key; *p; p++)
    {
        hash ^= (uint64_t)(unsigned char)(*p);
        hash *= FNV_PRIME;
    }
    return hash;
}

struct hashtable *hashtable_new(void)
{
    struct hashtable *ht = malloc(sizeof(struct hashtable));
    assert(ht != NULL);

    ht->size = 0;

    return ht;
}

void hashtable_free(struct hashtable *ht)
{
    if (ht == NULL)
    {
        return;
    }

    for (unsigned int bucket = 0; bucket < NUM_BUCKETS; bucket++)
    {
        struct hashtable_entry *entry = ht->buckets[bucket];
        free_entry(entry);
    }

    free(ht);
}

void *hashtable_get(struct hashtable *ht, const char *key)
{
    assert(ht != NULL);
    assert(key != NULL);

    struct hashtable_entry *entry = ht->buckets[hash_key(key) % NUM_BUCKETS];

    while (entry != NULL && strcmp(entry->key, key) != 0)
    {
        entry = entry->next;
    }

    if (entry == NULL)
    {
        return NULL;
    }

    return entry->value;
}

struct hashtable *hashtable_set(struct hashtable *ht, const char *key, const void *value)
{
    assert(ht != NULL);
    assert(key != NULL);

    uint64_t bucket = hash_key(key) % NUM_BUCKETS;
    struct hashtable_entry *prev_entry = ht->buckets[bucket];
    struct hashtable_entry *curr_entry = prev_entry;

    while (curr_entry != NULL)
    {
        if (strcmp(curr_entry->key, key) == 0)
        {
            curr_entry->value = (void *)value;
            return ht;
        }

        prev_entry = curr_entry;
        curr_entry = curr_entry->next;
    }

    struct hashtable_entry *new_entry = malloc(sizeof(struct hashtable_entry));
    assert(new_entry != NULL);

    new_entry->key = strdup(key);
    assert(new_entry->key != NULL);

    new_entry->value = (void *)value;

    if (prev_entry == NULL)
    {
        ht->buckets[bucket] = new_entry;
    }
    else
    {
        prev_entry->next = new_entry;
    }

    ht->size++;
    return ht;
}

size_t hashtable_size(struct hashtable *ht)
{
    return ht->size;
}
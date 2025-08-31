#ifndef HASHTABLE_H
#define HASHTABLE_H

struct hashtable;

struct hashtable *hashtable_new(void);
void hashtable_free(struct hashtable *ht);
void *hashtable_get(struct hashtable *ht, const char *key);
struct hashtable *hashtable_set(struct hashtable *ht, const char *key, const void *value);
unsigned long hashtable_size(struct hashtable *ht);

#endif
/*
Day 13: Knights of the Dinner Table
https://adventofcode.com/2015/day/13
*/

#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../lib/hashtable.h"
#include "../lib/permutation.h"

// #define USE_EXAMPLE

#ifdef USE_EXAMPLE
#define PART1_ANSWER 330
#define PART2_ANSWER -1 // Not provided
#else
#define PART1_ANSWER 618
#define PART2_ANSWER 601
#endif

char *get_seated_name(const char *line);
char *get_next_to_name(const char *line);
int get_happiness_change(const char *line);
bool str_array_contains(char **array, size_t length, char *str);
char *get_pair_key(const char *seated, const char *next_to);
int compare_names(const void *a, const void *b);
int get_arrangement_happiness(char **names, size_t num_names, struct hashtable *happiness_ht);

int main(void)
{
    char *me = strdup("Thomas Cleary");
    assert(me != NULL);

    // Hashtable value needs a pointer to int
    int values[BUFSIZ];
    values[0] = 0;
    size_t num_values = 1;

    char *names[BUFSIZ];
    size_t num_names = 0;

    struct hashtable *happiness_ht = hashtable_new();
    assert(happiness_ht != NULL);

    char line[BUFSIZ];
    while (fgets(line, BUFSIZ, stdin) != NULL)
    {
        line[strcspn(line, "\n")] = '\0'; // trim the trailing newline

        char *seated = get_seated_name(line);
        char *next_to = get_next_to_name(line);
        int happiness = get_happiness_change(line);
        char *seated_next_to_key = get_pair_key(seated, next_to);
        char *me_seated_key = get_pair_key(me, seated);
        char *seated_me_key = get_pair_key(seated, me);

        if (!str_array_contains(names, num_names, seated))
        {
            char *seated_dup = strdup(seated);
            assert(seated_dup != NULL);
            names[num_names++] = seated_dup;
        }
        else
        {
            free(seated);
        }
        if (!str_array_contains(names, num_names, next_to))
        {
            char *next_to_dup = strdup(next_to);
            assert(next_to_dup != NULL);
            names[num_names++] = next_to_dup;
        }
        else
        {
            free(next_to);
        }

        values[num_values] = happiness;

        hashtable_set(happiness_ht, seated_next_to_key, &(values[num_values]));
        hashtable_set(happiness_ht, me_seated_key, &(values[0]));
        hashtable_set(happiness_ht, seated_me_key, &(values[0]));
        free(seated_next_to_key);
        free(me_seated_key);
        free(seated_me_key);

        num_values++;
    }

    qsort(names, num_names, sizeof(char *), compare_names);

    int optimal_happiness = get_arrangement_happiness(names, num_names, happiness_ht);

    while (next_permutation(names, num_names))
    {
        int arrangement_happiness = get_arrangement_happiness(names, num_names, happiness_ht);

        if (optimal_happiness < arrangement_happiness)
        {
            optimal_happiness = arrangement_happiness;
        }
    }

    printf("Part 1: %d\n", optimal_happiness);
    assert(optimal_happiness == PART1_ANSWER);

    names[num_names++] = me;
    qsort(names, num_names, sizeof(char *), compare_names);

    optimal_happiness = get_arrangement_happiness(names, num_names, happiness_ht);

    while (next_permutation(names, num_names))
    {
        int arrangement_happiness = get_arrangement_happiness(names, num_names, happiness_ht);

        if (optimal_happiness < arrangement_happiness)
        {
            optimal_happiness = arrangement_happiness;
        }
    }

    printf("Part 2: %d\n", optimal_happiness);
    assert(optimal_happiness == PART2_ANSWER);

    hashtable_free(happiness_ht);
    for (size_t i = 0; i < (num_names - 1); i++)
    {
        free(names[i]);
    }

    return 0;
}

char *get_seated_name(const char *line)
{
    char *line_dup = strdup(line);
    assert(line_dup != NULL);

    return strsep(&line_dup, " ");
}

char *get_next_to_name(const char *line)
{
    char *line_dup = strdup(line);
    assert(line_dup != NULL);
    char *to_free = line_dup;

    char *next_to;
    while (line_dup != NULL)
    {
        next_to = strsep(&line_dup, " ");
    }

    next_to[strcspn(next_to, ".")] = '\0'; // trim the trailing fullstop

    char *name = strdup(next_to);
    free(to_free);

    return name;
}

int get_happiness_change(const char *line)
{
    char *line_dup = strdup(line);
    assert(line_dup != NULL);
    char *to_free = line_dup;

    char *gain_lose;
    for (int i = 0; i < 3; i++)
    {
        gain_lose = strsep(&line_dup, " ");
    }

    bool is_gain = strcmp(gain_lose, "gain") == 0;

    char *happiness = strsep(&line_dup, " ");
    int value;
    sscanf(happiness, "%d", &value);

    free(to_free);

    return is_gain ? value : -value;
}

bool str_array_contains(char **array, size_t length, char *str)
{
    for (int i = 0; i < length; i++)
    {
        if (strcmp(array[i], str) == 0)
        {
            return true;
        }
    }

    return false;
}

char *get_pair_key(const char *seated, const char *next_to)
{
    assert(seated != NULL);
    assert(next_to != NULL);

    size_t key_length = strlen(seated) + strlen(next_to) + 2; // "from:to"
    char *key = malloc(key_length);
    assert(key != NULL);

    strcpy(key, seated);
    strcat(key, ":");
    strcat(key, next_to);

    return key;
}

int compare_names(const void *a, const void *b)
{
    return strcmp(*(char **)a, *(char **)b);
}

int get_arrangement_happiness(char **names, size_t num_names, struct hashtable *happiness_ht)
{
    assert(num_names > 1);

    int total_happiness = 0;

    for (size_t i = 0; i < num_names; i++)
    {
        char *seated = names[i];
        char *next_to = names[(i + 1) % num_names];

        char *key = get_pair_key(seated, next_to);
        int *happiness = hashtable_get(happiness_ht, key);
        assert(happiness != NULL);
        total_happiness += *happiness;
        free(key);

        key = get_pair_key(next_to, seated);
        happiness = hashtable_get(happiness_ht, key);
        assert(happiness != NULL);
        total_happiness += *happiness;
        free(key);
    }

    return total_happiness;
}

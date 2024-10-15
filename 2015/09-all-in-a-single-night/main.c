/*
Day 9: All in a Single Night
https://adventofcode.com/2015/day/9
*/

#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "distance.h"
#include "../lib/hashtable.h"

// #define USE_EXAMPLE

#ifdef USE_EXAMPLE
#define PART1_ANSWER 605
#define PART2_ANSWER 982
#else
#define PART1_ANSWER 207
#define PART2_ANSWER 804
#endif

bool str_array_contains(char **array, size_t length, char *str);
char *get_distances_key(char *from, char *to);
int compare_locations(const void *a, const void *b);

/**
 * see: https://en.wikipedia.org/wiki/Permutation#Generation_in_lexicographic_order
 */
bool next_permutation(char **strings, size_t num_strings);
int get_route_distance(char **locations, size_t num_locations, struct hashtable *distances_ht);

int main(void)
{
    // Hashtable value needs a pointer to int
    int distances[BUFSIZ];
    size_t num_distances = 0;

    char *locations[BUFSIZ];
    size_t num_locations = 0;

    struct hashtable *distances_ht = hashtable_new();
    assert(distances_ht != NULL);

    char line[BUFSIZ];
    while (fgets(line, BUFSIZ, stdin) != NULL)
    {
        line[strcspn(line, "\n")] = '\0'; // trim the trailing newline

        struct distance *distance = distance_new(line);

        if (!str_array_contains(locations, num_locations, distance->from))
        {
            char *from = strdup(distance->from);
            assert(from != NULL);
            locations[num_locations++] = from;
        }
        if (!str_array_contains(locations, num_locations, distance->to))
        {
            char *to = strdup(distance->to);
            assert(to != NULL);
            locations[num_locations++] = to;
        }

        distances[num_distances] = distance->distance;

        char *key = get_distances_key(distance->from, distance->to);
        hashtable_set(distances_ht, key, &(distances[num_distances]));
        free(key);

        key = get_distances_key(distance->to, distance->from);
        hashtable_set(distances_ht, key, &(distances[num_distances]));
        free(key);

        num_distances++;

        distance_free(distance);
    }

    qsort(locations, num_locations, sizeof(char *), compare_locations);

    int shortest_route_distance = get_route_distance(locations, num_locations, distances_ht);
    int longest_route_distance = shortest_route_distance;

    while (next_permutation(locations, num_locations))
    {
        int route_distance = get_route_distance(locations, num_locations, distances_ht);

        if (route_distance < shortest_route_distance)
        {
            shortest_route_distance = route_distance;
        }
        else if (route_distance > longest_route_distance)
        {
            longest_route_distance = route_distance;
        }
    }

    hashtable_free(distances_ht);
    for (size_t i = 0; i < num_locations; i++)
    {
        free(locations[i]);
    }

    printf("Shortest route: %d\n", shortest_route_distance);
    printf("Longest route: %d\n", longest_route_distance);

    assert(shortest_route_distance == PART1_ANSWER);
    assert(longest_route_distance == PART2_ANSWER);

    return 0;
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

char *get_distances_key(char *from, char *to)
{
    assert(from != NULL);
    assert(to != NULL);

    size_t key_length = strlen(from) + strlen(to) + 2; // "from:to"
    char *key = malloc(key_length);
    assert(key != NULL);

    strcpy(key, from);
    strcat(key, ":");
    strcat(key, to);

    return key;
}

int compare_locations(const void *a, const void *b)
{
    return strcmp(*(char **)a, *(char **)b);
}

bool next_permutation(char **strings, size_t num_strings)
{
    // 1. Find the largest index k such that a[k] < a[k + 1]. If no such index exists,
    // the permutation is the last permutation.
    int k = num_strings - 2;
    while (k >= 0 && strcmp(strings[k], strings[k + 1]) >= 0)
    {
        k--;
    }

    if (k < 0)
    {
        return false;
    }

    // 2. Find the largest index l greater than k such that a[k] < a[l].
    int l = k + 1;
    int largest_l = -1;

    while (l < num_strings)
    {
        if (strcmp(strings[k], strings[l]) < 0)
        {
            largest_l = l;
        }

        l++;
    }

    assert(largest_l != -1);

    // 3. Swap the value of a[k] with that of a[l].
    char *tmp = strings[k];
    strings[k] = strings[largest_l];
    strings[largest_l] = tmp;

    // 4. Reverse the sequence from a[k + 1] up to and including the final element a[n].
    int left = k + 1;
    int right = num_strings - 1;

    while (left < right)
    {
        char *tmp = strings[left];
        strings[left] = strings[right];
        strings[right] = tmp;

        left++;
        right--;
    }

    return true;
}

int get_route_distance(char **locations, size_t num_locations, struct hashtable *distances_ht)
{
    assert(num_locations > 1);

    unsigned int distance = 0;

    for (size_t i = 0; i < (num_locations - 1); i++)
    {
        char *from = locations[i];
        char *to = locations[i + 1];
        char *key = get_distances_key(from, to);

        unsigned int *dist = hashtable_get(distances_ht, key);
        assert(dist != NULL);

        distance += *dist;
        free(key);
    }

    return distance;
}
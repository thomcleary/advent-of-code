#include <assert.h>
#include <string.h>

#include "permutation.h"

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
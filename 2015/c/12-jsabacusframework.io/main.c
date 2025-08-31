/*
Day 12: JSAbacusFramework.io
https://adventofcode.com/2015/day/12
*/

#include <assert.h>
#include <stdio.h>
#include <string.h>

// #define USE_EXAMPLE

#ifdef USE_EXAMPLE
#define PART1_ANSWER 9
#define PART2_ANSWER 0
#else
#define PART1_ANSWER 119433
#define PART2_ANSWER 0
#endif

int main(void)
{
    int total = 0;
    int nums_read;
    int num;

    while ((nums_read = scanf(" %d", &num)) != EOF)
    {
        if (nums_read > 0)
        {
            total += num;
        }
        else
        {
            scanf("%*c");
        }
    }

    printf("Part 1: %d\n", total);
    assert(total == PART1_ANSWER);

    return 0;
}

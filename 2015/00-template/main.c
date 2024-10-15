/*
Day 0: Template
https://adventofcode.com/2015/day/0
*/

#include <assert.h>
#include <stdio.h>
#include <string.h>

#define USE_EXAMPLE

#ifdef USE_EXAMPLE
#define PART1_ANSWER 0
#define PART2_ANSWER 0
#else
#define PART1_ANSWER 0
#define PART2_ANSWER 0
#endif

int main(void)
{
    char line[BUFSIZ];

    while (fgets(line, BUFSIZ, stdin) != NULL)
    {
        line[strcspn(line, "\n")] = '\0'; // trim the trailing newline
        printf("%s\n", line);
    }

    return 0;
}

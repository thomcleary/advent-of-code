/*
Day 8: Matchsticks
https://adventofcode.com/2015/day/8
*/

#include <assert.h>
#include <stdio.h>
#include <string.h>

// #define USE_EXAMPLE

#ifdef USE_EXAMPLE
#define PART1_ANSWER 12
#else
#define PART1_ANSWER 1371
#endif

int main(void)
{
    char line[BUFSIZ];

    unsigned long code_chars = 0;
    unsigned long memory_chars = 0;

    while (fgets(line, BUFSIZ, stdin) != NULL)
    {
        line[strcspn(line, "\n")] = '\0'; // trim the trailing newline

        char *start = line + 1; // skip the opening '"' character
        code_chars += 1;

        start[strlen(start) - 1] = '\0'; // trim the trailing '"' character
        code_chars += 1;

        while (*start)
        {
            if (*start == '\\')
            {
                if (*(start + 1) != 'x')
                {
                    code_chars += 2;
                    memory_chars += 1;
                    start += 2;
                    continue;
                }

                code_chars += 4;
                memory_chars += 1;
                start += 4;
                continue;
            }

            code_chars += 1;
            memory_chars += 1;
            start += 1;
        }
    }

    unsigned long part1_answer = code_chars - memory_chars;
    printf("(code_chars - memory_chars) = %lu\n", part1_answer);
    assert(part1_answer == PART1_ANSWER);

    return 0;
}

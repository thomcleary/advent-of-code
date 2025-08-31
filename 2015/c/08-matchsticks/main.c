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
#define PART2_ANSWER 19
#else
#define PART1_ANSWER 1371
#define PART2_ANSWER 2117
#endif

int main(void)
{
    char line[BUFSIZ];

    unsigned long code_chars = 0;
    unsigned long memory_chars = 0;
    unsigned long encoded_code_chars = 0;

    while (fgets(line, BUFSIZ, stdin) != NULL)
    {
        line[strcspn(line, "\n")] = '\0'; // trim the trailing newline

        char *start = line + 1; // skip the opening '"' character
        code_chars += 1;
        encoded_code_chars += 2; // '\"'

        start[strlen(start) - 1] = '\0'; // trim the trailing '"' character
        code_chars += 1;
        encoded_code_chars += 2; // '\"'

        while (*start)
        {
            if (*start == '\\')
            {
                if (*(start + 1) != 'x')
                {
                    code_chars += 2;
                    memory_chars += 1;
                    encoded_code_chars += 2; // +1 for the escape char '\' and +1 for the char being escaped
                    start += 2;
                    continue;
                }

                code_chars += 4;
                memory_chars += 1;
                encoded_code_chars += 1; // +1 for the escape char '\' before the 'x'
                start += 4;
                continue;
            }

            code_chars += 1;
            memory_chars += 1;
            start += 1;
        }
    }

    unsigned long part1_answer = code_chars - memory_chars;
    printf("Part 1: %lu\n", part1_answer);
    assert(part1_answer == PART1_ANSWER);

    unsigned long part2_answer = encoded_code_chars;
    printf("Part 2: %lu\n", part2_answer);
    assert(part2_answer == PART2_ANSWER);

    return 0;
}

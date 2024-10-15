/*
Day 10: Elves Look, Elves Say
https://adventofcode.com/2015/day/10
*/

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// #define USE_EXAMPLE

#ifdef USE_EXAMPLE
#define INPUT "1"
#define ITERATIONS 5
#define PART1_ANSWER 6
#define PART2_ANSWER 0
#else
#define INPUT "1113122113"
#define ITERATIONS 40
#define PART1_ANSWER 360154
#define PART2_ANSWER 0
#endif

void concat_n_chars(char *str, int n, char ch)
{
    size_t str_len = strlen(str);
    str[str_len] = '0' + n;
    str[str_len + 1] = ch;
    str[str_len + 2] = '\0';
}

int main(void)
{
    char *sequence = strdup(INPUT);
    assert(sequence != NULL);

    for (int i = 0; i < ITERATIONS; i++)
    {
        char *next_sequence = malloc((strlen(sequence) * 2) + 1);
        assert(next_sequence != NULL);

        char *curr = sequence;
        char prev = *sequence;
        unsigned int consecutive = 0;

        while (*curr)
        {
            if (*curr != prev)
            {
                concat_n_chars(next_sequence, consecutive, prev);
                consecutive = 1;
            }
            else
            {
                consecutive++;
            }
            prev = *curr;
            curr++;
        }
        concat_n_chars(next_sequence, consecutive, prev);

        free(sequence);
        sequence = next_sequence;
    }

    size_t sequence_len = strlen(sequence);
    printf("Length of sequence after %d iterations: %lu\n", ITERATIONS, sequence_len);
    assert(sequence_len == PART1_ANSWER);

    free(sequence);

    return 0;
}

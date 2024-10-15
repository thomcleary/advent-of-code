/*
Day 10: Elves Look, Elves Say
https://adventofcode.com/2015/day/10
*/

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define INPUT "1113122113"
#define PART1_ITERATIONS 40
#define PART1_ANSWER 360154
#define PART2_ITERATIONS 50
#define PART2_ANSWER 5103798

char *look_and_say(char *input, unsigned int iterations)
{
    char *sequence = strdup(input);
    assert(sequence != NULL);

    for (int i = 0; i < iterations; i++)
    {
        char *next_sequence = malloc((strlen(sequence) * 2) + 1);
        assert(next_sequence != NULL);

        next_sequence[0] = '\0';
        int next_sequence_len = 0; // Manually keep track of strlen, calling strlen is slow

        char *curr = sequence;
        char prev = *sequence;
        unsigned int consecutive = 0;

        while (*curr)
        {
            if (*curr != prev)
            {
                next_sequence[next_sequence_len++] = '0' + consecutive;
                next_sequence[next_sequence_len++] = prev;
                next_sequence[next_sequence_len] = '\0';
                consecutive = 1;
            }
            else
            {
                consecutive++;
            }
            prev = *curr;
            curr++;
        }
        next_sequence[next_sequence_len++] = '0' + consecutive;
        next_sequence[next_sequence_len++] = prev;
        next_sequence[next_sequence_len] = '\0';

        free(sequence);
        sequence = next_sequence;
    }

    return sequence;
}

int main(void)
{
    char *part1_sequence = look_and_say(INPUT, PART1_ITERATIONS);
    size_t sequence_len = strlen(part1_sequence);
    printf("Length of sequence after %d iterations: %lu\n", PART1_ITERATIONS, sequence_len);
    assert(sequence_len == PART1_ANSWER);

    char *part2_sequence = look_and_say(INPUT, PART2_ITERATIONS);
    sequence_len = strlen(part2_sequence);
    printf("Length of sequence after %d iterations: %lu\n", PART2_ITERATIONS, sequence_len);
    assert(sequence_len == PART2_ANSWER);

    free(part1_sequence);
    free(part2_sequence);

    return 0;
}

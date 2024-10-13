/*
Day 1: Not Quite Lisp (Part 1)
https://adventofcode.com/2015/day/1
*/

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>

#define ANSWER 74

int main(void)
{
    char ch;
    int floor = 0;

    while ((ch = getchar()) != EOF)
    {
        if (ch == '(')
        {
            floor++;
        }
        else if (ch == ')')
        {
            floor--;
        }
        else
        {
            printf("Unexpected char: %c\n", ch);
            exit(EXIT_FAILURE);
        }
    }

    printf("Floor: %d\n", floor);

    assert(floor == ANSWER);

    return 0;
}
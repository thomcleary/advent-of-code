/*
Day 1: Not Quite Lisp (Part 1)
https://adventofcode.com/2015/day/1
*/

#include <stdlib.h>
#include <stdio.h>

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

    return 0;
}
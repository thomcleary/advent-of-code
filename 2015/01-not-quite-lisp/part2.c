/*
Day 1: Not Quite Lisp (Part 2)
https://adventofcode.com/2015/day/1
*/

#include <stdlib.h>
#include <stdio.h>

int main(void)
{
    char ch;
    int floor = 0;
    int position = 0;

    while (floor >= 0 && (ch = getchar()) != EOF)
    {
        position++;

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

    printf("Position: %d\n", position);

    return 0;
}
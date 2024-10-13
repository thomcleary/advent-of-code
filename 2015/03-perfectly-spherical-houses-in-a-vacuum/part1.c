/*
Day 3: Perfectly Spherical Houses in a Vacuum (Part 1)
https://adventofcode.com/2015/day/3
*/

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

#define ANSWER 2572

#define ROWS 1024
#define COLS 1024

int main(void)
{
    // Should probably learn how to build a hashmap but cbf rn
    bool delivered_to[ROWS][COLS] = {false};

    char move;
    int houses_delivered_to = 1;

    int row = ROWS / 2;
    int col = COLS / 2;

    delivered_to[row][col] = true;

    while ((move = getchar()) != EOF)
    {
        switch (move)
        {
        case '^':
            if (row == 0)
            {
                printf("House grid row lower bound exceeded.\n");
                exit(EXIT_FAILURE);
            }
            row--;
            break;
        case '>':
            if (col == COLS - 1)
            {
                printf("House grid column upper bound exceeded.\n");
                exit(EXIT_FAILURE);
            }
            col++;
            break;
        case 'v':
            if (row == ROWS - 1)
            {
                printf("House grid row upper bound exceeded.\n");
                exit(EXIT_FAILURE);
            }
            row++;
            break;
        case '<':
            if (col == 0)
            {
                printf("House grid column lower bound exceeded.\n");
                exit(EXIT_FAILURE);
            }
            col--;
            break;
        default:
            printf("Invalid move: %c\n", move);
            exit(EXIT_FAILURE);
        }

        if (!delivered_to[row][col])
        {
            delivered_to[row][col] = true;
            houses_delivered_to++;
        }
    }

    printf("Houses that recieved at least 1 present: %d\n", houses_delivered_to);

    assert(houses_delivered_to == ANSWER);

    return 0;
}
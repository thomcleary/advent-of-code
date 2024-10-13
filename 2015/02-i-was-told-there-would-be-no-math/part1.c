/*
Day 2: I Was Told There Would Be No Math (Part 1)
https://adventofcode.com/2015/day/2
*/

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>

#define ANSWER 1598415

int smallest_side(int x, int y, int z);

int main(void)
{
    int wrapping_paper_feet = 0;
    int length, height, width;

    while (scanf("%dx%dx%d", &length, &height, &width) != EOF)
    {
        int lw = length * width;
        int wh = width * height;
        int hl = height * length;

        wrapping_paper_feet += (2 * lw) + (2 * wh) + (2 * hl);
        wrapping_paper_feet += smallest_side(lw, wh, hl);
    }

    printf("Wrapping paper required (feet): %d\n", wrapping_paper_feet);

    assert(wrapping_paper_feet == ANSWER);

    return 0;
}

int smallest_side(int x, int y, int z)
{
    if (x < y && x < z)
    {
        return x;
    }

    return y < z ? y : z;
}

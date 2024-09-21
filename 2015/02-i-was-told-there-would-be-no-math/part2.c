/*
Day 2: I Was Told There Would Be No Math (Part 2)
https://adventofcode.com/2015/day/2
*/

#include <stdlib.h>
#include <stdio.h>

int smallest_perimeter(int x, int y, int z);
int min(int x, int y);
int max(int x, int y);

int main(void)
{
    long ribbon_feet = 0;
    int length, height, width;

    while (scanf("%dx%dx%d", &length, &height, &width) != EOF)
    {
        int volume = length * height * width;
        int perimeter = smallest_perimeter(length, height, width);

        ribbon_feet += volume + perimeter;
    }

    printf("Ribbon required (feet): %ld\n", ribbon_feet);

    return 0;
}

int smallest_perimeter(int x, int y, int z)
{
    return 2 * (min(x, y) + min(max(x, y), z));
}

int min(int x, int y)
{
    return x < y ? x : y;
}

int max(int x, int y)
{
    return x > y ? x : y;
}

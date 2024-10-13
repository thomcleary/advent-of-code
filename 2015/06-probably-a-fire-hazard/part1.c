/*
Day 6: Probably a Fire Hazard (Part 1)
https://adventofcode.com/2015/day/6
*/

#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

#define ANSWER 377891

#define GRID_SIZE 1000

struct point
{
    int x, y;
};

enum Command
{
    ON,
    OFF,
    TOGGLE
};

struct instruction
{
    enum Command command;
    struct point upper_left;
    struct point lower_right;
};

struct instruction line_to_instruction(char *line);
void do_instruction(struct instruction i, bool grid[GRID_SIZE][GRID_SIZE]);

int main(void)
{
    bool grid[GRID_SIZE][GRID_SIZE] = {false};
    char line[BUFSIZ];

    while (fgets(line, BUFSIZ, stdin) != NULL)
    {
        do_instruction(line_to_instruction(line), grid);
    }

    int lights_on = 0;
    for (int i = 0; i < GRID_SIZE; i++)
    {
        for (int j = 0; j < GRID_SIZE; j++)
        {
            if (grid[i][j])
            {
                lights_on++;
            }
        }
    }

    printf("Lights on: %d\n", lights_on);

    assert(lights_on == ANSWER);

    return 0;
}

struct point token_to_point(char *token)
{
    int x, y;
    sscanf(token, "%d,%d", &x, &y);
    return (struct point){.x = x, .y = y};
}

struct instruction line_to_instruction(char *line)
{
    enum Command command;
    struct point upper_left;
    struct point lower_right;

    char *token = strtok(line, " ");

    if (strcmp(token, "toggle") == 0)
    {
        command = TOGGLE;
    }
    else
    {
        token = strtok(NULL, " ");
        command = strcmp(token, "on") == 0 ? ON : OFF;
    }

    upper_left = token_to_point(strtok(NULL, " "));
    strtok(NULL, " "); // ignore 'through'
    lower_right = token_to_point(strtok(NULL, " "));

    return (struct instruction){
        .command = command,
        .upper_left = upper_left,
        .lower_right = lower_right};
}

void do_instruction(struct instruction i, bool grid[GRID_SIZE][GRID_SIZE])
{
    for (int row = i.upper_left.x; row <= i.lower_right.x; row++)
    {
        for (int col = i.upper_left.y; col <= i.lower_right.y; col++)
        {

            grid[row][col] = i.command == TOGGLE ? !grid[row][col] : i.command == ON;
        }
    }
}

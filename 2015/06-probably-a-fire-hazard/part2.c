/*
Day 6: Probably a Fire Hazard (Part 2)
https://adventofcode.com/2015/day/6
*/

#include <stdio.h>
#include <string.h>

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
void do_instruction(struct instruction i, unsigned int grid[GRID_SIZE][GRID_SIZE]);

int main(void)
{
    unsigned int grid[GRID_SIZE][GRID_SIZE] = {0};
    char line[BUFSIZ];

    while (fgets(line, BUFSIZ, stdin) != NULL)
    {
        do_instruction(line_to_instruction(line), grid);
    }

    int brightness = 0;
    for (int i = 0; i < GRID_SIZE; i++)
    {
        for (int j = 0; j < GRID_SIZE; j++)
        {
            brightness += grid[i][j];
        }
    }

    printf("Brightness: %d\n", brightness);

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

void do_instruction(struct instruction i, unsigned int grid[GRID_SIZE][GRID_SIZE])
{
    for (int row = i.upper_left.x; row <= i.lower_right.x; row++)
    {
        for (int col = i.upper_left.y; col <= i.lower_right.y; col++)
        {
            switch (i.command)
            {
            case ON:
                grid[row][col] += 1;
                break;
            case TOGGLE:
                grid[row][col] += 2;
                break;
            default:
                if (grid[row][col] > 0)
                {
                    grid[row][col] -= 1;
                }
            }
        }
    }
}

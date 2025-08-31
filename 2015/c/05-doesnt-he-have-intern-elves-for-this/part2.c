/*
Day 5: Doesn't He Have Intern-Elves For This? (Part 2)
https://adventofcode.com/2015/day/5
*/

#include <assert.h>
#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

#define ANSWER 51

#define MAX_STR_LEN 16

bool is_nice(char *str);

int main(void)
{
    char str[MAX_STR_LEN + 2]; // +1 for '\n', +1 for '\0'
    int nice = 0;

    while (fgets(str, MAX_STR_LEN + 2, stdin) != NULL)
    {
        if (is_nice(str))
        {
            nice++;
        }
    }

    printf("Nice strings: %d\n", nice);

    assert(nice == ANSWER);

    return 0;
}

void to_lower(char *str)
{
    while (*str)
    {
        *str = tolower(*str);
        str++;
    }
}

bool is_nice(char *str)
{
    to_lower(str);

    bool has_repeat_char = false;
    bool has_repeat_substr = false;

    char needle[3] = {'\0'};

    while (*(str + 2) && *(str + 2) != '\n')
    {
        char *haystack = str + 2;

        if (*str == *haystack)
        {
            has_repeat_char = true;
        }

        needle[0] = *str;
        needle[1] = *(str + 1);

        if (strstr(haystack, needle) != NULL)
        {
            has_repeat_substr = true;
        }

        str++;
    }

    return has_repeat_char && has_repeat_substr;
}
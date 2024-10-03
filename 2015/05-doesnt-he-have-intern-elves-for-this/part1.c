/*
Day 5: Doesn't He Have Intern-Elves For This? (Part 1)
https://adventofcode.com/2015/day/5
*/

#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>

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

    return 0;
}

bool is_naughty_substring(char prev, char curr)
{
    if (prev != 'a' && prev != 'c' && prev != 'p' && prev != 'x')
    {
        return false;
    }

    return ((curr - 1) == prev);
}

bool is_vowel(char ch)
{
    switch (ch)
    {
    case 'a':
    case 'e':
    case 'i':
    case 'o':
    case 'u':
        return true;
    default:
        return false;
    }
}

bool is_nice(char *str)
{
    int vowels = 0;
    int has_consecutive_char = false;

    char prev = '\0';

    while (*str && *str != '\n')
    {
        char curr = tolower(*str);

        if (is_naughty_substring(prev, curr))
        {
            return false;
        }

        if (is_vowel(curr))
        {
            vowels++;
        }

        if (curr == prev)
        {
            has_consecutive_char = true;
        }

        prev = curr;
        str++;
    }

    return has_consecutive_char && vowels >= 3;
}
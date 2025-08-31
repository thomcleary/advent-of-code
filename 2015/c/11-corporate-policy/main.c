/*
Day 11: Corporate Policy
https://adventofcode.com/2015/day/11
*/

#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define INPUT "vzbxkghb"
#define PART1_ANSWER "vzbxxyzz"
#define PART2_ANSWER "vzcaabcc"

bool is_illegal_char(char ch)
{
    return ch == 'i' || ch == 'o' || ch == 'l';
}

bool increment_password_char(char *password, size_t pos)
{
    password[pos] += 1;

    if (password[pos] > 'z')
    {
        if (pos == 0)
        {
            password[pos] = 'z';
            return false;
        }
        password[pos] = 'a';
        return increment_password_char(password, pos - 1);
    }

    return true;
}

bool increment_password(char *password)
{
    size_t password_len = strlen(password);

    for (size_t i = 0; i < password_len; i++)
    {
        char curr = password[i];
        if (is_illegal_char(curr))
        {
            password[i] += 1;
        }
    }

    return increment_password_char(password, password_len - 1);
}

bool is_straight(char first, char second, char third)
{
    return (first + 1) == (second) && (second + 1) == third;
}

bool is_valid_password(char *password)
{
    size_t password_len = strlen(password);

    if (password_len < 4)
    {
        return false;
    }

    bool has_straight = false;

    size_t head = 0;
    size_t tail = head + 3;

    while (tail < password_len)
    {
        char first = password[head];
        char second = password[head + 1];
        char third = password[head + 2];
        char fourth = password[head + 3];

        if (is_illegal_char(first) || is_illegal_char(second) || is_illegal_char(third) || is_illegal_char(fourth))
        {
            return false;
        }

        has_straight = has_straight || is_straight(first, second, third) || is_straight(second, third, fourth);

        head += 2;
        tail += 2;
    }

    if (!has_straight)
    {
        return false;
    }

    head = 0;
    tail = head + 1;
    unsigned short pairs = 0;

    while (tail < password_len && pairs < 2)
    {
        if (password[head] == password[tail])
        {
            pairs += 1;
            head += 2;
            tail += 2;
        }
        else
        {
            head += 1;
            tail += 1;
        }
    }

    return pairs >= 2;

    // Don't need to handle odd length passwords as input length is 8
    // abcd[efgh]i
    // abcdef[ghi]
    // Odd length password would leave a final set of 3 chars to be checked
}

char *get_next_password(char *current_password)
{
    char *password = strdup(current_password);
    assert(password != NULL);

    while (!is_valid_password(password))
    {
        if (!increment_password(password))
        {
            printf("Cannot increment password [%s] any further\n", password);
            break;
        }
    }

    return password;
}

int main(void)
{
    char *next_password = get_next_password(INPUT);
    printf("Next password: %s\n", next_password);
    assert(strcmp(next_password, PART1_ANSWER) == 0);

    increment_password(next_password);
    char *next_next_password = get_next_password(next_password);
    printf("Next next password: %s\n", next_next_password);
    assert(strcmp(next_next_password, PART2_ANSWER) == 0);

    free(next_password);
    free(next_next_password);

    return 0;
}

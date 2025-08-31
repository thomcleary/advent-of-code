#include <assert.h>
#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "instruction.h"

static char *LOGIC_GATES[] = {[AND] = "AND", [OR] = "OR", [RSHIFT] = "RSHIFT", [LSHIFT] = "LSHIFT", [NOT] = "NOT"};

static bool is_signal(const char *source)
{
    while (*source)
    {
        if (!isdigit(*source))
        {
            return false;
        }
        source++;
    }
    return true;
}

static struct gate_input *new_gate_input(char *input)
{
    assert(input != NULL);

    struct gate_input *gate_input = malloc(sizeof(struct gate_input));
    assert(gate_input != NULL);

    if (is_signal(input))
    {
        gate_input->kind = SIGNAL;
        sscanf(input, "%hu", &(gate_input->value.signal));
    }
    else
    {
        gate_input->kind = WIRE;
        gate_input->value.wire = input;
    }

    return gate_input;
}

void print_instruction(const struct instruction *instruction)
{
    assert(instruction != NULL);

    switch (instruction->source_kind)
    {
    case SIGNAL:
    {
        printf("[%hu] ", instruction->source.signal);
        break;
    }
    case WIRE:
    {
        printf("[%s] ", instruction->source.wire);
        break;
    }
    case GATE:
    {
        struct gate_input *left = instruction->source.gate->left_input;
        struct gate_input *right = instruction->source.gate->right_input;
        assert(right != NULL);

        if (left)
        {
            if (left->kind == SIGNAL)
            {
                printf("[%hu] ", left->value.signal);
            }
            else
            {
                printf("[%s] ", left->value.wire);
            }
        }

        printf("[%s] ", LOGIC_GATES[instruction->source.gate->kind]);

        if (right->kind == SIGNAL)
        {
            printf("[%hu] ", right->value.signal);
        }
        else
        {
            printf("[%s] ", right->value.wire);
        }

        break;
    }
    default:
    {
        printf("Invalid wire source_kind: [%d]\n", instruction->source_kind);
        exit(EXIT_FAILURE);
    }
    }

    printf("-> [%s]\n", instruction->wire);
}

struct instruction *to_instruction(const char *str)
{
    struct instruction *instruction = malloc(sizeof(struct instruction));
    assert(instruction != NULL);

    char *str_copy = strdup(str);
    assert(str_copy != NULL);
    char *str_to_free = str_copy;

    char *source = strsep(&str_copy, "-");

    // strsep updates the str pointer to the position after the separator
    // +2 to skip over the remaining '> ' before the wire ID
    char *wire = str_copy + 2;

    char *source_parts[3];
    char *part;
    int num_parts = 0;
    while ((part = strsep(&source, " ")) && source != NULL)
    {
        source_parts[num_parts] = strdup(part);
        assert(source_parts[num_parts] != NULL);
        num_parts++;
    }

    assert(num_parts != 0);
    assert(num_parts <= 3);

    instruction->wire = strdup(wire);
    assert(instruction->wire != NULL);

    if (num_parts == 1)
    {
        if (is_signal(source_parts[0]))
        {
            instruction->source_kind = SIGNAL;
            sscanf(source_parts[0], "%hu", &(instruction->source.signal));
        }
        else
        {
            instruction->source_kind = WIRE;
            instruction->source.wire = source_parts[0];
        }
    }
    else
    {
        instruction->source_kind = GATE;

        struct gate *gate = malloc(sizeof(struct gate));
        assert(gate != NULL);

        if (num_parts == 2)
        {
            gate->kind = NOT;
            gate->left_input = NULL;
            gate->right_input = new_gate_input(source_parts[1]);
        }
        else
        {
            char *left = source_parts[0];
            char *kind = source_parts[1];
            char *right = source_parts[2];

            if (strcmp(kind, "AND") == 0)
            {
                gate->kind = AND;
            }
            else if (strcmp(kind, "OR") == 0)
            {
                gate->kind = OR;
            }
            else if (strcmp(kind, "RSHIFT") == 0)
            {
                gate->kind = RSHIFT;
            }
            else if (strcmp(kind, "LSHIFT") == 0)
            {
                gate->kind = LSHIFT;
            }
            else
            {
                printf("Invalid gate kind: [%s]\n", kind);
                exit(EXIT_FAILURE);
            }

            gate->left_input = new_gate_input(left);
            gate->right_input = new_gate_input(right);
        }

        instruction->source.gate = gate;
    }

    free(str_to_free);

    return instruction;
}
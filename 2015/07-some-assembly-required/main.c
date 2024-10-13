/*
Day 7: Some Assembly Required (Part 1)
https://adventofcode.com/2015/day/7
*/

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "hashtable.h"
#include "instruction.h"

static unsigned short get_signal(const char *wire, struct hashtable *instruction_ht);
static unsigned short get_gate_signal(struct gate *gate, struct hashtable *instruction_ht);

int main(void)
{
    char line[BUFSIZ];

    struct hashtable *ht = hashtable_create();
    assert(ht != NULL);

    while (fgets(line, BUFSIZ, stdin) != NULL)
    {
        line[strcspn(line, "\n")] = '\0';

        struct instruction *instruction = to_instruction(line);
        assert(instruction != NULL);

        void *result = hashtable_set(ht, instruction->wire, instruction);
        assert(result != NULL);
    }

    const char *wire = "a";
    printf("----------------------------------------\n");
    printf("Part 1\n");
    printf("----------------------------------------\n");
    printf("Wire [%s]'s signal: %hu\n", wire, get_signal(wire, ht));

    hashtable_destroy(ht);

    return 0;
}

static unsigned short get_signal(const char *wire, struct hashtable *instruction_ht)
{
    struct instruction *instruction = hashtable_get(instruction_ht, wire);

    if (instruction == NULL)
    {
        printf("Instruction for wire [%s] not found.\n", wire);
        exit(EXIT_FAILURE);
    }

    if (instruction->source_kind == SIGNAL)
    {
        return instruction->source.signal;
    }

    unsigned short signal;

    if (instruction->source_kind == WIRE)
    {
        signal = get_signal(instruction->source.wire, instruction_ht);
    }
    else
    {
        signal = get_gate_signal(instruction->source.gate, instruction_ht);
    }

    instruction->source_kind = SIGNAL;
    instruction->source.signal = signal;
    return signal;
}

static unsigned short get_gate_signal(struct gate *gate, struct hashtable *instruction_ht)
{
    struct gate_input *left = gate->left_input;
    struct gate_input *right = gate->right_input;

    unsigned short left_signal;
    if (gate->kind != NOT)
    {
        assert(left != NULL);
        left_signal = left->kind == SIGNAL ? left->value.signal : get_signal(left->value.wire, instruction_ht);
    }

    assert(right != NULL);
    unsigned short right_signal = right->kind == SIGNAL ? right->value.signal : get_signal(right->value.wire, instruction_ht);

    switch (gate->kind)
    {
    case AND:
    {
        return left_signal & right_signal;
    }
    case OR:
    {
        return left_signal | right_signal;
    }
    case RSHIFT:
    {
        return left_signal >> right_signal;
    }
    case LSHIFT:
    {
        return left_signal << right_signal;
    }
    case NOT:
    {
        return ~right_signal;
    }
    default:
    {
        printf("Invalid gate kind [%d]\n", gate->kind);
        exit(EXIT_FAILURE);
    }
    }
}

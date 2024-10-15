/*
Day 7: Some Assembly Required
https://adventofcode.com/2015/day/7
*/

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../lib/hashtable.h"
#include "instruction.h"

#define PART1_ANSWER 956
#define PART2_ANSWER 40149

static unsigned short get_signal(const char *wire, struct hashtable *instruction_ht);
static unsigned short get_gate_signal(struct gate *gate, struct hashtable *instruction_ht);

int main(void)
{
    char line[BUFSIZ];

    struct hashtable *ht_part1 = hashtable_new();
    struct hashtable *ht_part2 = hashtable_new();

    assert(ht_part1 != NULL);
    assert(ht_part2 != NULL);

    while (fgets(line, BUFSIZ, stdin) != NULL)
    {
        line[strcspn(line, "\n")] = '\0';

        struct instruction *instruction_part1 = to_instruction(line);
        assert(instruction_part1 != NULL);
        void *result_part1 = hashtable_set(ht_part1, instruction_part1->wire, instruction_part1);
        assert(result_part1 != NULL);

        struct instruction *instruction_part2 = malloc(sizeof(struct instruction));
        assert(instruction_part2 != NULL);
        memcpy(instruction_part2, instruction_part1, sizeof(struct instruction));
        void *result_part2 = hashtable_set(ht_part2, instruction_part2->wire, instruction_part2);
        assert(result_part2 != NULL);
    }

    const char *target_wire = "a";
    unsigned short target_wire_signal = get_signal(target_wire, ht_part1);

    printf("----------------------------------------\n");
    printf("Part 1\n");
    printf("----------------------------------------\n");
    printf("- Wire [%s]'s signal: %hu\n\n", target_wire, target_wire_signal);

    const char *override_wire = "b";
    struct instruction *override_instruction = hashtable_get(ht_part2, override_wire);
    assert(override_instruction != NULL);
    override_instruction->source_kind = SIGNAL;
    override_instruction->source.signal = target_wire_signal;
    unsigned short recomputed_target_wire_signal = get_signal(target_wire, ht_part2);

    printf("----------------------------------------\n");
    printf("Part 2\n");
    printf("----------------------------------------\n");
    printf("- Overriding [%s]'s signal to [%hu].\n", override_instruction->wire, target_wire_signal);
    printf("- Recomputing wire [%s]'s signal.\n", target_wire);
    printf("- Wire [%s]'s signal: %hu\n\n", target_wire, recomputed_target_wire_signal);

    hashtable_free(ht_part1);
    hashtable_free(ht_part2);

    assert(target_wire_signal == PART1_ANSWER);
    assert(recomputed_target_wire_signal == PART2_ANSWER);

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

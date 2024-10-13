#ifndef PART1_H
#define PART1_H

enum source
{
    SIGNAL,
    WIRE,
    GATE,
};

enum logic_gate
{
    AND,
    OR,
    RSHIFT,
    LSHIFT,
    NOT,
};

struct gate_input
{
    enum source kind;
    union
    {
        unsigned short signal;
        char *wire;
    } value;
};

struct gate
{
    enum logic_gate kind;
    struct gate_input *left_input;
    struct gate_input *right_input;
};

struct instruction
{
    char *wire;
    enum source source_kind;
    union
    {
        unsigned short signal;
        char *wire;
        struct gate *gate;
    } source;
};

void print_instruction(const struct instruction *instruction);
struct instruction *to_instruction(const char *str);

#endif
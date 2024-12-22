/*
Day 17: Chronospatial Computer
https://adventofcode.com/2024/day/17
*/

#define _DEFAULT_SOURCE
#include <assert.h>
#include <errno.h>
#include <inttypes.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "../lib/ansi.h"
#include "../lib/aoc.h"
#include "../lib/txt.h"

// #define DEBUG
#define ANIMATION_SPEED_MS 66

// #define USE_EXAMPLE

#ifdef USE_EXAMPLE
#define PART1_INPUT_FILENAME "example-input.part1.txt"
#define PART1_ANSWER "4,6,3,5,6,3,5,2,1,0"
#define PART2_INPUT_FILENAME "example-input.part2.txt"
#define PART2_ANSWER "0,3,5,4,3,0"
#else
#define PART1_INPUT_FILENAME "puzzle-input.part1.txt"
#define PART1_ANSWER "4,1,7,6,4,1,0,2,7"
#define PART2_INPUT_FILENAME "puzzle-input.part2.txt"
#define PART2_ANSWER ""
#endif

typedef enum OpCode {
  ADV = 0,
  BXL = 1,
  BST = 2,
  JNZ = 3,
  BXC = 4,
  OUT = 5,
  BDV = 6,
  CDV = 7,
} OpCode;

typedef struct {
  OpCode op_code;
  uint8_t operand;
} Instruction;

typedef struct {
  Instruction *instructions;
  size_t length;
} Program;

typedef struct {
  char *id;
  uint64_t register_a, register_b, register_c;
  uint64_t instruction_pointer;

  uint8_t *stdout;
  size_t stdout_length;
  size_t stdout_size;
} Computer;

static const char *OP_CODE_NAME[] = {
    [ADV] = "adv", [BXL] = "bxl", [BST] = "bst", [JNZ] = "jnz",
    [BXC] = "bxc", [OUT] = "out", [BDV] = "bdv", [CDV] = "cdv"};

void computer_print(const Computer *computer) {
  ansi_esc(ANSI_CODE_BOLD);
  printf("Computer [%s]\n", computer->id);
  ansi_reset();

  ansi_esc(ANSI_CODE_FG_RED);
  printf("[A]  %" PRIu64 "\n", computer->register_a);
  ansi_reset();

  ansi_esc(ANSI_CODE_FG_GREEN);
  printf("[B]  %" PRIu64 "\n", computer->register_b);
  ansi_reset();

  ansi_esc(ANSI_CODE_FG_YELLOW);
  printf("[C]  %" PRIu64 "\n", computer->register_c);
  ansi_reset();

  ansi_esc(ANSI_CODE_FG_BLUE);
  printf("[IP] %" PRIu64 "\n", computer->instruction_pointer);
  ansi_reset();

  printf("\n");
}

void program_print(Program *program, uint64_t instruction_pointer) {
  ansi_esc(ANSI_CODE_BOLD);
  printf("Program\n");
  ansi_reset();

  for (size_t i = 0; i < program->length; i++) {
    Instruction instruction = program->instructions[i];

    if (i == instruction_pointer) {
      ansi_esc(ANSI_CODE_BOLD);
      ansi_esc(ANSI_CODE_FG_BLUE);
    } else {
      ansi_esc(ANSI_CODE_FAINT);
    }

    printf("[%zu] %s %d\n", i, OP_CODE_NAME[instruction.op_code],
           instruction.operand);

    ansi_reset();
  }

  printf("\n");
}

char *computer_stdout_str(Computer *computer, char *stdout_str,
                          size_t stdout_str_len) {
  size_t ch = 0;
  size_t instruction = 0;

  while (ch < stdout_str_len) {
    stdout_str[ch++] = (char)(computer->stdout[instruction++]);
    stdout_str[ch++] = ',';
  }

  stdout_str[ch - 1] = '\0';

  return stdout_str;
}

void stdout_print(Computer *computer) {
  ansi_esc(ANSI_CODE_BOLD);
  printf("STDOUT\n");
  ansi_reset();

  ansi_esc(ANSI_CODE_FAINT);
  if (computer->stdout_length == 0) {
    printf("EMPTY");
  } else {
    size_t stdout_str_len = (computer->stdout_length * 2 - 1) + 1;
    char stdout_str[stdout_str_len];
    computer_stdout_str(computer, stdout_str, stdout_str_len);
    printf("%s", stdout_str);
  }
  printf("\n\n");
  ansi_reset();
}

void print_state(Computer *computer, Program *program) {
  computer_print(computer);
  program_print(program, computer->instruction_pointer);
  stdout_print(computer);
}

void computer_free(Computer *computer) {
  free(computer->stdout);
}

Computer computer_init(const Txt *txt, char *id) {
  assert(txt->num_lines == 3 && "invalid register information");

  Computer computer = {.id = id, .instruction_pointer = 0};

  int matched =
      sscanf(txt->lines[0], "Register A: %" PRIu64, &computer.register_a);
  matched +=
      sscanf(txt->lines[1], "Register B: %" PRIu64, &computer.register_b);
  matched +=
      sscanf(txt->lines[2], "Register C: %" PRIu64, &computer.register_c);
  assert(matched == 3 && "scanf failed");

  computer.stdout_size = BUFSIZ;
  computer.stdout_length = 0;
  computer.stdout = malloc(sizeof(*computer.stdout) * computer.stdout_size);
  assert(computer.stdout != NULL && "malloc failed");

  return computer;
}

void program_free(Program *program) {
  free(program->instructions);
}

Program program_parse(const Txt *txt) {
  assert(txt->num_lines == 1 && "invalid program information");
  char *line = txt->lines[0];

  char program_str[strlen(line)];
  sscanf(line, "Program: %s", program_str);

  size_t num_instructions = (strlen(program_str) + 1) / 4;
  Instruction *instructions = malloc(sizeof(*instructions) * num_instructions);

  size_t instruction_num = 0;
  OpCode op_code;
  uint8_t operand;

  char *curr_char = program_str;
  while (*curr_char) {
    op_code = (OpCode)*curr_char - '0';
    curr_char++;
    curr_char++; // skip ','
    operand = (uint8_t)*curr_char - '0';
    curr_char++;

    if (*curr_char) {
      curr_char++; // skip ','
    }

    instructions[instruction_num++] =
        (Instruction){.op_code = op_code, .operand = operand};
  }
  assert(instruction_num == num_instructions && "invalid instructions");

  return (Program){.instructions = instructions, .length = instruction_num};
}

uint64_t combo_operand(Computer *computer, uint8_t operand) {
  switch (operand) {
  case 0:
  case 1:
  case 2:
  case 3:
    return operand;
  case 4:
    return computer->register_a;
  case 5:
    return computer->register_b;
  case 6:
    return computer->register_c;
  case 7:
    fprintf(
        stderr,
        "Combo operand 7 is reserved and will not appear in valid programs.");
    break;
  default:
    fprintf(stderr, "Invalid combo operand [%d]\n", operand);
  }

  exit(EXIT_FAILURE);
}

uint64_t adv(Computer *computer, uint8_t operand) {
  uint64_t denominator = 1 << (combo_operand(computer, operand)); // 2^x

  computer->register_a /= denominator;

  return computer->instruction_pointer + 1;
}

uint64_t bxl(Computer *computer, uint8_t operand) {
  computer->register_b ^= operand;

  return computer->instruction_pointer + 1;
}

uint64_t bst(Computer *computer, uint8_t operand) {
  computer->register_b = combo_operand(computer, operand) % 8;

  return computer->instruction_pointer + 1;
}

uint64_t jnz(Computer *computer, uint8_t operand) {
  if (computer->register_a == 0) {
    return computer->instruction_pointer + 1;
  }

  return operand;
}

// For legacy reasons, this instruction reads an operand but ignores it.
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunused-parameter"
uint64_t bxc(Computer *computer, uint8_t operand) {
  computer->register_b ^= computer->register_c;

  return computer->instruction_pointer + 1;
}
#pragma clang diagnostic pop

uint64_t out(Computer *computer, uint8_t operand) {
  uint8_t output = combo_operand(computer, operand) % 8;

  if (computer->stdout_length == computer->stdout_size) {
    computer->stdout_size *= 2;
    computer->stdout = realloc(computer->stdout, sizeof(*computer->stdout) *
                                                     computer->stdout_size);
    assert(computer->stdout != NULL && "realloc failed");
  }

  computer->stdout[computer->stdout_length++] = output + '0';

  return computer->instruction_pointer + 1;
}

uint64_t bdv(Computer *computer, uint8_t operand) {
  uint64_t numerator = computer->register_a;
  uint64_t denominator = 1 << (combo_operand(computer, operand)); // 2^x

  computer->register_b = numerator / denominator;

  return computer->instruction_pointer + 1;
}

uint64_t cdv(Computer *computer, uint8_t operand) {
  uint64_t numerator = computer->register_a;
  uint64_t denominator = 1 << (combo_operand(computer, operand)); // 2^x

  computer->register_c = numerator / denominator;

  return computer->instruction_pointer + 1;
}

typedef uint64_t InstructionFn(Computer *computer, uint8_t operand);

static InstructionFn *INSTRUCTION_FNS[] = {
    [ADV] = adv, [BXL] = bxl, [BST] = bst, [JNZ] = jnz,
    [BXC] = bxc, [OUT] = out, [BDV] = bdv, [CDV] = cdv};

void computer_run(Computer *computer, Program *program) {
  while (computer->instruction_pointer < program->length) {
    ansi_clear();
    print_state(computer, program);
#ifdef DEBUG
    printf("[ENTER] to continue...\n");
    getchar();
#else
    usleep(ANIMATION_SPEED_MS * 1000);
#endif
    Instruction instruction =
        program->instructions[computer->instruction_pointer];

    computer->instruction_pointer =
        INSTRUCTION_FNS[instruction.op_code](computer, instruction.operand);
  }

  ansi_clear();
  print_state(computer, program);
}

char *run(uint8_t part, char *filename) {
  Txt *txt = txt_read_file(filename);
  SplitTxt *split_txt = txt_split(txt, "");
  assert(split_txt->num_txts == 2 && "invalid puzzle input");

  Computer computer =
      computer_init(&split_txt->txts[0], part == 1 ? "Part 1" : "Part 2");
  Program program = program_parse(&split_txt->txts[1]);

  computer_run(&computer, &program);

  size_t stdout_str_len = (computer.stdout_length * 2 - 1) + 1;
  char stdout_str[stdout_str_len];
  computer_stdout_str(&computer, stdout_str, stdout_str_len);

  program_free(&program);
  computer_free(&computer);
  split_txt_free(split_txt);
  txt_free(txt);

  return strdup(stdout_str);
}

int main(void) {
  char *part1_output = run(1, PART1_INPUT_FILENAME);
  char *part2_output = run(2, PART2_INPUT_FILENAME);

  print_day(17, "Chronospatial Computer");
  print_part_str(1, part1_output, PART1_ANSWER);
  print_part_str(2, part2_output, PART2_ANSWER);

  free(part1_output);
  free(part2_output);

  return 0;
}

/*
Day 13: Claw Contraption
https://adventofcode.com/2024/day/13
*/

#define _DEFAULT_SOURCE
#include <assert.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include "../lib/aoc.h"
#include "../lib/txt.h"

// #define USE_EXAMPLE

#ifdef USE_EXAMPLE
#define PART1_ANSWER 480
#define PART2_ANSWER 875318608908
#else
#define PART1_ANSWER 31065
#define PART2_ANSWER 93866170395343
#endif

typedef struct {
  int64_t _1, _2;
} Vec2;

typedef struct {
  Vec2 a, b, c;
} LinearSystem;

typedef struct {
  bool success;
  int64_t x, y;
} CramersRuleResult;

typedef struct {
  struct {
    int64_t x_diff, y_diff;
  } A, B;
  struct {
    int64_t x, y;
  } prize;
} ClawMachine;

typedef struct {
  ClawMachine *machines;
  size_t num_machines;
} ClawMachineList;

typedef struct {
  bool win;
  int64_t a_pushes, b_pushes;
} PlayResult;

ClawMachineList claw_machine_list_parse(Txt *txt) {
  SplitTxt *split_txt = txt_split(txt, "");

  ClawMachineList list = {
      .machines = malloc(sizeof(*list.machines) * split_txt->num_txts),
      .num_machines = split_txt->num_txts,
  };

  for (size_t i = 0; i < split_txt->num_txts; i++) {
    Txt txt_machine = split_txt->txts[i];
    assert(txt_machine.num_lines == 3 && "invalid machine description");

    ClawMachine machine;
    int matched =
        sscanf(txt_machine.lines[0], "Button A: X+%" PRId64 ", Y+%" PRId64,
               &machine.A.x_diff, &machine.A.y_diff);
    assert(matched == 2 && "invalid A button");
    matched =
        sscanf(txt_machine.lines[1], "Button B: X+%" PRId64 ", Y+%" PRId64,
               &machine.B.x_diff, &machine.B.y_diff);
    assert(matched == 2 && "invalid B button");
    matched = sscanf(txt_machine.lines[2], "Prize: X=%" PRId64 ", Y=%" PRId64,
                     &machine.prize.x, &machine.prize.y);
    assert(matched == 2 && "invalid prize");

    list.machines[i] = machine;
  }

  split_txt_free(split_txt);

  return list;
}

int64_t determinant(Vec2 a, Vec2 b) {
  return (a._1 * b._2) - (b._1 * a._2);
}

// https://en.wikipedia.org/wiki/Cramer%27s_rule
CramersRuleResult cramers_rule(LinearSystem ls) {
  int64_t det = determinant(ls.a, ls.b);

  if (det == 0) {
    return (CramersRuleResult){.success = false};
  }

  return (CramersRuleResult){.success = true,
                             .x = determinant(ls.c, ls.b) / det,
                             .y = determinant(ls.a, ls.c) / det};
}

PlayResult play(ClawMachine *machine) {
  CramersRuleResult cramer_result = cramers_rule(
      (LinearSystem){.a = {._1 = machine->A.x_diff, ._2 = machine->A.y_diff},
                     .b = {._1 = machine->B.x_diff, ._2 = machine->B.y_diff},
                     .c = {._1 = machine->prize.x, ._2 = machine->prize.y}});

  if (!cramer_result.success) {
    return (PlayResult){.win = false};
  }

  int64_t claw_position_x = (cramer_result.x * machine->A.x_diff) +
                            (cramer_result.y * machine->B.x_diff);

  int64_t claw_position_y = (cramer_result.x * machine->A.y_diff) +
                            (cramer_result.y * machine->B.y_diff);

  bool win = claw_position_x == machine->prize.x &&
             claw_position_y == machine->prize.y;

  return (PlayResult){
      .win = win, .a_pushes = cramer_result.x, .b_pushes = cramer_result.y};
}

int64_t claw_machines_test(ClawMachineList machines) {
  int64_t tokens_required = 0;

  for (size_t i = 0; i < machines.num_machines; i++) {
    PlayResult result = play(&machines.machines[i]);

    if (result.win) {
      tokens_required += (3 * result.a_pushes) + result.b_pushes;
    }
  }

  return tokens_required;
}

void claw_machines_fix_measurements(ClawMachineList machines) {
  const int64_t prize_adjustment = 10000000000000;

  for (size_t i = 0; i < machines.num_machines; i++) {
    machines.machines[i].prize.x += prize_adjustment;
    machines.machines[i].prize.y += prize_adjustment;
  }
}

int main(void) {
  Txt *txt = txt_read(stdin);
  ClawMachineList claw_machines = claw_machine_list_parse(txt);

  int64_t tokens_required = claw_machines_test(claw_machines);
  claw_machines_fix_measurements(claw_machines);
  int64_t tokens_required_after_fix = claw_machines_test(claw_machines);

  free(claw_machines.machines);
  txt_free(txt);

  print_day(13, "Claw Contraption");
  print_part(1, (uint64_t)tokens_required, PART1_ANSWER);
  print_part(2, (uint64_t)tokens_required_after_fix, PART2_ANSWER);

  return 0;
}

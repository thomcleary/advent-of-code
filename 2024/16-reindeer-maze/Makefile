CC = gcc
CC_FLAGS = -std=c17 -Wall -Wextra -Werror -pedantic -Wshadow -Wuninitialized -Wformat -Wconversion

TARGET = main

LIBS = ansi aoc binary-heap txt types
OBJS = $(addprefix ../lib/, $(addsuffix .o, $(LIBS))) $(TARGET).o

$(TARGET): $(OBJS)
	$(CC) $(CC_FLAGS) -o $@ $^

%.o: %.c %.h $(LIB_HEADERS)
	$(CC) $(CC_FLAGS) -c $< -o $@

../lib/%.o: ../lib/%.c ../lib/%.h $(LIB_HEADERS)
	$(CC) $(CC_FLAGS) -c $< -o $@

.PHONY: clean
clean:
	rm $(TARGET) $(OBJS)

CC = gcc
CC_FLAGS = -std=c17 -Wall -Werror

TARGET = main
OBJS = ../lib/aoc.o $(TARGET).o

$(TARGET): $(OBJS)
	$(CC) $(CC_FLAGS) -o $@ $^

%.o: %.c %.h
	$(CC) $(CC_FLAGS) -c $< -o $@

../lib/%.o: ../lib/%.c ../lib/%.h
	$(CC) $(CC_FLAGS) -c $< -o $@

clean:
	rm $(TARGET) $(OBJS)

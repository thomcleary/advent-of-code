CC = gcc
CC_FLAGS = -std=c17 -Wall -Werror

TARGET = main
OBJS = ../lib/aoc.o ../lib/hashtable.o
HEADERS = $(TARGET).h ../lib/aoc.h ../lib/hashtable.h

$(TARGET): $(TARGET).o $(OBJS)
	$(CC) $(CC_FLAGS) -o $(TARGET) $(TARGET).o $(OBJS)

../lib/aoc.o: ../lib/aoc.c ../lib/aoc.h
	$(CC) $(CC_FLAGS) -c $< -o $@

../lib/hashtable.o: ../lib/hashtable.c ../lib/hashtable.h
	$(CC) $(CC_FLAGS) -c $< -o $@

%.o: %.c $(HEADERS)
	$(CC) $(CC_FLAGS) -c $<

clean:
	rm $(TARGET) $(OBJS) *.o

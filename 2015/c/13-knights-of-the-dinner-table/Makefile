CC = cc 
CC_FLAGS = -std=c99 -Wall -Werror

TARGET = main
OBJS = ../lib/hashtable.o ../lib/permutation.o
HEADERS = ../lib/hashtable.h ../lib/permutation.h

$(TARGET): $(TARGET).o $(OBJS)
	$(CC) $(CC_FLAGS) -o $(TARGET) $(TARGET).o $(OBJS)

../lib/hashtable.o: ../lib/hashtable.c ../lib/hashtable.h
	$(CC) $(CC_FLAGS) -c $< -o $@

../lib/permutation.o: ../lib/permutation.c ../lib/permutation.h
	$(CC) $(CC_FLAGS) -c $< -o $@

%.o: %.c $(HEADERS)
	$(CC) $(CC_FLAGS) -c $<

clean:
	rm $(TARGET) $(TARGET).o $(OBJS)
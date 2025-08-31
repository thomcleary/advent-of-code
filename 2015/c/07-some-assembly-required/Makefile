CC = cc 
CC_FLAGS = -std=c99 -Wall -Werror

TARGET = main
OBJS = instruction.o ../lib/hashtable.o
HEADERS = instruction.h ../lib/hashtable.h

$(TARGET): $(TARGET).o $(OBJS)
	$(CC) $(CC_FLAGS) -o $(TARGET) $(TARGET).o $(OBJS)

instruction.o: instruction.c instruction.h
	$(CC) $(CC_FLAGS) -c $<

../lib/hashtable.o: ../lib/hashtable.c ../lib/hashtable.h
	$(CC) $(CC_FLAGS) -c $< -o $@

%.o: %.c $(HEADERS)
	$(CC) $(CC_FLAGS) -c $<

clean:
	rm $(TARGET) $(TARGET).o $(OBJS)
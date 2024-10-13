CC = cc 
CC_FLAGS = -std=c99 -Wall -Werror

TARGET = main
OBJS = instruction.o hashtable.o
HEADERS = instruction.h hashtable.h

$(TARGET)1: $(TARGET).o $(OBJS)
	$(CC) $(CC_FLAGS) -o $(TARGET) $(TARGET).o $(OBJS)

instruction.o: instruction.c instruction.h
	$(CC) $(CC_FLAGS) -c $<

hashtable.o: hashtable.c hashtable.h
	$(CC) $(CC_FLAGS) -c $<

%.o: %.c $(HEADERS)
	$(CC) $(CC_FLAGS) -c $<

clean:
	rm $(TARGET) *.o
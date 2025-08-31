CC = cc 
CC_FLAGS = -std=c99 -Wall -Werror

TARGET = main
OBJS = distance.o ../lib/hashtable.o ../lib/permutation.o
HEADERS = distance.h ../lib/hashtable.h ../lib/permutation.h

$(TARGET): $(TARGET).o $(OBJS)
	$(CC) $(CC_FLAGS) -o $(TARGET) $(TARGET).o $(OBJS)

distance.o: distance.c distance.h
	$(CC) $(CC_FLAGS) -c $<

../lib/hashtable.o: ../lib/hashtable.c ../lib/hashtable.h
	$(CC) $(CC_FLAGS) -c $< -o $@

../lib/permutation.o: ../lib/permutation.c ../lib/permutation.h
	$(CC) $(CC_FLAGS) -c $< -o $@

%.o: %.c $(HEADERS)
	$(CC) $(CC_FLAGS) -c $<

clean:
	rm $(TARGET) $(TARGET).o $(OBJS)
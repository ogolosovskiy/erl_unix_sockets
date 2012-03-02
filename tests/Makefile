CC      = g++
CFLAGS  = -I/.
LDFLAGS = 

all: clean server_test

server_test: server_test.o
	$(CC) -o $@ $^ $(LDFLAGS)

server_test.o: server_test.cpp
	$(CC) -c $(CFLAGS) $<

.PHONY: clean cleanest

clean:
	rm -f *.o
	rm -f server_test


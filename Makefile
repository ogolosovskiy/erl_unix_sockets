
ERL_CFLAGS ?= -I/usr/local/lib/erlang/lib/erl_interface-3.7.4/include -I/usr/local/lib/erlang/erts-5.8.4/include 
DRV_LDFLAGS ?= -shared  -L/usr/local/lib/erlang/lib/erl_interface-3.7.4/lib -lerl_interface -lei

all:
	g++ $(ERL_CFLAGS) -o ./c_src/pipe_drv.o  -c -fpic ./c_src/pipe_drv.cpp 
	g++ ./c_src/pipe_drv.o -o ./priv/pipe_drv.so $(DRV_LDFLAGS)
clean:
	rm -f ./priv/*
	rm -f ./c_src/*.o


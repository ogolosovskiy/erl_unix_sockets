
ERLANG_INSTALL=/usr/local
ERTS_PATH=$ERLANG_INSTALL/lib/erlang/erts-5.8.3
ERL_INTERFACE_PATH=$ERLANG_INSTALL/lib/erlang/lib/erl_interface-3.7.3

DRIVER_SRC = ./cpp_src/*.cpp
DRIVER_INC = 
DRIVER_DEST = ./priv/

all:
	g++ -I/usr/local/lib/erlang/lib/erl_interface-3.7.4/include/ -I/usr/local/lib/erlang/erts-5.8.4/include/ -o ./cpp_src/pipe_drv.o  -c -fpic ./cpp_src/pipe_drv.cpp 
	ld -shared ./cpp_src/pipe_drv.o -o ./priv/pipe_drv.so -L/usr/local/lib/erlang/lib/erl_interface-3.7.4/lib/ -lei

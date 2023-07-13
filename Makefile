MIX = mix

ERLANG_PATH = $(shell erl -eval 'io:format("~s", [lists:concat([code:root_dir(), "/erts-", erlang:system_info(version), "/include"])]), halt().' -noshell)
CFLAGS += -I$(ERLANG_PATH)
CFLAGS += -I c_src/libbtc -I c_src/libbtc/src -I c_src/libbtc/include/btc

ifeq ($(wildcard deps/libbtc),)
	LIB_PATH = ../libbtc
else
	LIB_PATH = deps/libbtc
endif

CFLAGS += -I$(LIB_PATH)/src

ifneq ($(OS),Windows_NT)
	CFLAGS += -fPIC

	ifeq ($(shell uname),Darwin)
		LDFLAGS += -dynamiclib -undefined dynamic_lookup
	endif
endif

LDFLAGS += c_src/libbtc/.libs/libbtc.a

.PHONY: clean

all: priv/libbtc_ex_nif.so

priv/libbtc_ex_nif.so: c_src/libbtc_ex_nif.c
	c_src/build_deps.sh
	$(CC) $(CFLAGS) -shared -o $@ c_src/libbtc_ex_nif.c $(LDFLAGS)

clean:
	$(MIX) clean
	c_src/build_deps.sh clean
	$(MAKE) -C $(LIB_PATH) clean
	$(RM) priv/libbtc_ex_nif.so


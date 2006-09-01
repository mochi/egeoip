
include ../support/include.mk

include ../vsn.mk
VSN=$(ESWF_VSN)

EBIN_FILES = $(ERL_OBJECTS) $(ERL_DOCUMENTS) ../ebin/egeoip.app
MODULES = $(ERL_SOURCES:%.erl=%)

all: $(EBIN_FILES)

debug:
	$(MAKE) DEBUG=-DDEBUG

clean:
	rm -rf $(EBIN_FILES)

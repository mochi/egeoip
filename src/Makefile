include ../support/include.mk

all: $(EBIN_FILES)

local: $(ERL_OBJECTS_LOCAL)

debug:
	$(MAKE) DEBUG=-DDEBUG

clean:
	rm -rf $(EBIN_FILES)

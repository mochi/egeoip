## -*- makefile -*-

######################################################################
## Erlang

PREFIX := /usr/local
ERL	:= $(PREFIX)/bin/erl
ERLC	:= $(PREFIX)/bin/erlc

ERLDIR	:= $(PREFIX)/lib/erlang
ERL_C_INCLUDE_DIR := $(ERLDIR)/usr/include

ERLC_FLAGS := -W -I ../include

ifndef no_debug_info
  ERLC_FLAGS += +debug_info
endif

ifdef debug
  ERLC_FLAGS += -Ddebug
endif

EBIN_DIR := ../ebin
DOC_DIR  := ../doc
EMULATOR := beam

ERL_SOURCES := $(wildcard *.erl)
ERL_HEADERS := $(wildcard *.hrl) $(wildcard ../include/*.hrl)
ERL_OBJECTS := $(ERL_SOURCES:%.erl=$(EBIN_DIR)/%.$(EMULATOR))
ERL_DOCUMENTS := $(ERL_SOURCES:%.erl=$(DOC_DIR)/%.html)

# Hmm, don't know if you are supposed to like this better... ;-)
APPSCRIPT = '$$vsn=shift; $$mods=""; while(@ARGV){ $$_=shift; s/^([A-Z].*)$$/\'\''$$1\'\''/; $$mods.=", " if $$mods; $$mods .= $$_; } while(<>) { s/%VSN%/$$vsn/; s/%MODULES%/$$mods/; print; }'

../ebin/%.app: %.app.src ../vsn.mk Makefile                 
	perl -e $(APPSCRIPT) "$(VSN)" $(MODULES) < $< > $@  

$(EBIN_DIR)/%.$(EMULATOR): %.erl
	$(ERLC) $(ERLC_FLAGS) -o $(EBIN_DIR) $<

# generate documentation with edoc:
# this is still not the proper way to do it, but it works
# (see the wumpus application for an example)

$(DOC_DIR)/%.html: %.erl
	${ERL} -noshell \
          -pa ../../syntax_tools/ebin \
          -pa ../../edoc/ebin \
          -pa ../../xmerl/ebin \
	  -pa ../../ucs/ebin \
          -run edoc file $< -run init stop
	mv *.html $(DOC_DIR)

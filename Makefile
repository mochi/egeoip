REBAR ?= $(shell which rebar 2>/dev/null || which ./rebar)
REBAR_FLAGS ?=

all: compile

compile:
	$(REBAR) compile $(REBAR_FLAGS)

doc:
	$(REBAR) doc $(REBAR_FLAGS)

test: compile
	$(REBAR) eunit $(REBAR_FLAGS)

clean:
	$(REBAR) clean $(REBAR_FLAGS)

clean_plt:
	@rm -f _test/dialyzer_plt

build_plt: build-plt

build-plt:
	@ [ -d _test ] || mkdir _test
	$(REBAR) build-plt $(REBAR_FLAGS)

dialyzer:
	$(REBAR) dialyze $(REBAR_FLAGS)


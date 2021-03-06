.PHONY: all deps compile dialyze wait test console clean clean-db

APPS = dialyzer.apps
PLT = apps.plt

all: deps compile

deps:
	@ rebar get-deps

compile: deps
	@ rebar compile

dialyze: compile $(PLT)
	@ dialyzer --plt $(PLT) ebin \
	  -Wunmatched_returns \
	  -Wno_undefined_callbacks

$(PLT): dialyzer.apps
	@- dialyzer -q --build_plt --output_plt $(PLT) \
	   --apps $(shell cat $(APPS))

test: deps compile wait
	@ rebar skip_deps=true ct

console: compile
	@ erl -pa ebin deps/*/ebin

clean:
	@ rebar clean

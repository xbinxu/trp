.PHONY: deps rel

all: deps compile

compile:
	./rebar compile

deps:
	./rebar get-deps

clean:
	./rebar skip_deps=true clean 

distclean: clean devclean relclean
	./rebar delete-deps

rel: clean all
	./rebar -f generate
	
devclean:
	rm -rf dev/client dev/server
	
devrel: client server

client server:
	./rebar clean
	./rebar compile
	mkdir -p dev
	(cd rel && ../rebar -f generate target_dir=../dev/$@ overlay_vars=vars/$@.config)
	

	


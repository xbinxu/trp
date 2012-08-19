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
	
deploy: devrel 
	tar zvcf trps.tar.gz dev/server/lib/trp-1
	lftp -u app100629017,app100629017 ftp-cvmgz00.opencloud.qq.com:53000 -e 'mput trps.tar.gz; quit'
	
sync:
	rsync -r -e ssh --exclude=".git" --exclude="dev" --exclude="rel/trp" . xiaobin@192.168.12.14:~/trp
	


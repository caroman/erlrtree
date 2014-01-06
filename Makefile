
all: release escriptize

get-deps:
	rebar get-deps

compile:
	rebar compile

compile-escriptize:
	rebar compile escriptize

mkdirs:
	mkdir -p apps/rtree_client/lib/

escriptize: mkdirs compile-escriptize
	cp deps/erlgeom/priv/erlgeom.so apps/rtree_client/lib/
	cp deps/erlogr/priv/erlogr.so apps/rtree_client/lib/
	cp deps/erlosr/priv/erlosr.so apps/rtree_client/lib/

compile-generate:
	test -f rel/files || cd rel && rebar create-node nodeid=rtree_server && cd -
	rebar compile generate

release: compile-generate
	@echo "Start with: rel/rtree_server/bin/rtree_server start"
	@echo "Attach with: rel/rtree_server/bin/rtree_server attach"
	@echo "Stop with: rel/rtree_server/bin/rtree_server stop"

dialyzer_plt:
	-test ! -f ~/.dialyzer_plt && \
        dialyzer --build_plt --apps erts kernel stdlib mnesia syntax_tools

dialyzer-deps: compile dialyzer_plt
	-test ! -f deps/deps.plt && \
		cd deps && \
		dialyzer --build_plt --output_plt deps.plt \
		--plts ~/.dialyzer_plt \
    	-o dialyzer-deps.build \
    	--apps erlgeom erlogr erlosr lager goldrush poolboy && \
		cd -

dialyzer-apps: dialyzer-deps
	cd apps && \
	dialyzer --build_plt --output_plt apps.plt \
	    --plts ~/.dialyzer_plt ../deps/deps.plt \
        -o dialyzer-apps.build \
        --apps rtree_server rtree_client && \
	cd -

dialyzer: dialyzer-apps
	cd apps && \
	dialyzer \
        --plts ~/.dialyzer_plt ../deps/deps.plt apps.plt \
		--apps rtree_client rtree_server && \
	cd -

dialyzer-clean:
	rm -f deps/deps.plt
	rm -f apps/apps.plt

clean: dialyzer-clean
	rebar clean
	rm -f apps/rtree_server/priv/*
	rm -f apps/rtree_client/priv/*
	rm -f apps/rtree_client/lib/*
	cd deps/erlgeom/ && make clean && cd - 
	cd deps/erlogr/ && make clean && cd -
	cd deps/erlosr/ && make clean && cd -
	rm -rf rel/files
	rm -rf rel/rtree_server

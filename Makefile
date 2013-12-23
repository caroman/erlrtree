
all: release escriptize

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
	test -f rel/files || cd rel && rebar create-node nodeid=rtree && cd -
	rebar compile generate

release: compile-generate
	-echo "Start with: rel/rtree/bin/rtree start"
	-echo "Attach with: rel/rtree/bin/rtree attach"
	-echo "Stop with: rel/rtree/bin/rtree stop"

dialyzer_plt:
	test ! -f ~/.dialyzer_plt && \
        dialyzer --build_plt --apps erts kernel stdlib mnesia syntax_tools

dialyzer-deps:
	cd deps/erlgeom/ && make dialyzer-build && cd - 
	cd deps/erlogr/ && make dialyzer-build && cd -
	cd deps/erlosr/ && make dialyzer-build && cd -

dialyzer-build: compile dialyzer-deps
	dialyzer --build_plt --output_plt erlrtree.plt \
        -o dialyzer.build \
        -r apps/

dialyzer: dialyzer_plt dialyzer-build
	dialyzer \
        --plts ~/.dialyzer_plt \
            deps/erlgeom/erlgeom.plt \
            deps/erlogr/erlogr.plt \
            deps/erlosr/erlosr.plt \
            erlrtree.plt \
        -r apps/

dialyzer-clean:
	rm -f erlrtree.plt

clean: dialyzer-clean
	rebar clean
	rm -f apps/rtree/priv/*
	rm -f apps/rtree_client/priv/*
	rm -f apps/rtree_client/lib/*
	cd deps/erlgeom/ && make clean && cd - 
	cd deps/erlogr/ && make clean && cd -
	cd deps/erlosr/ && make clean && cd -

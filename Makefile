
all: release escriptize

compile:
	rebar compile

compile-escriptize:
	rebar compile escriptize

escriptize: compile-escriptize
	cp deps/erlgeom/priv/erlgeom.so apps/rtree_client/lib/
	cp deps/erlogr/priv/erlogr.so apps/rtree_client/lib/
	cp deps/erlosr/priv/erlosr.so apps/rtree_client/lib/

compile-release:
	./rebar compile release

release: compile-release
	echo "Attach with: rel/rtree/bin/rtree attach"

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

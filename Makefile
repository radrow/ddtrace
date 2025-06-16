.PHONY: all
all: deps
	mix escript.build


deps:
	mix deps.get


.PHONY: clean
clean:
	mix clean


.PHONY: cleanall
cleanall: clean
	rm -f ./ddmon
	rm -rf ./_build
	rm -rf ./deps


.PHONY: nuke
nuke: cleanall
	rm -f ./mix.lock
	rm -rf ./asdf

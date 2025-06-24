.PHONY: all
all: deps
	mix escript.build


deps:
	mix deps.get


.PHONY: docs
docs:
	git ls-files | grep -E '.md$' | xargs -I {} pandoc -t pdf {} -o {}.pdf


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

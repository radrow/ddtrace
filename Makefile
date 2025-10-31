.PHONY: all
all: deps
	mix escript.build

deps:
	mix deps.get


.PHONY: docs
docs:
	git ls-files | grep -e '\.md$$' | xargs -I {} pandoc "{}" -o "{}.pdf" -V geometry:margin=1cm --number-sections


.PHONY: clean
clean:
	mix clean


.PHONY: cleanall
cleanall: clean
	rm -f ./ddtrace
	rm -rf ./_build
	rm -rf ./deps


.PHONY: nuke
nuke: cleanall
	rm -f ./mix.lock
	rm -rf ./asdf

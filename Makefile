gh-actions:
	dune build @gh-actions

update-gitmodules:
	git submodule sync --recursive
	git submodule update --recursive --remote

.phony: update-gitmodules



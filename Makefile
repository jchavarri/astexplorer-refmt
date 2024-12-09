project_name = astexplorer-refmt

DUNE = opam exec -- dune
opam_file = $(project_name).opam

.PHONY: help
help:
	@echo "";
	@echo "List of available make commands";
	@echo "";
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "  \033[36m%-15s\033[0m %s\n", $$1, $$2}';
	@echo "";

.PHONY: build-dev
dev:
	$(DUNE) build -w

.PHONY: build-prod
prod:
	pnpm install
	$(DUNE) build
	mkdir -p dist
	chmod -R 777 dist
	pnpm build:prod

.PHONY: clean
clean: ## Clean artifacts
	$(DUNE) clean

.PHONY: exec
exec: ## Run the project
	$(DUNE) exec $(demo)

.PHONY: format
format: 
	DUNE_CONFIG__GLOBAL_LOCK=disabled $(DUNE) build @fmt --auto-promote

.PHONY: format-check
format-check:
	$(DUNE) build @fmt

.PHONY: create-switch
create-switch: ## Create opam switch
	opam switch create . 5.2.1 --deps-only --with-dev-setup -y

.PHONY: install
install:
	pnpm install
	$(DUNE) build @install
	opam install . --deps-only --with-test

.PHONY: init
init: create-switch install

.PHONY: demo
demo:
	pnpm install
	$(DUNE) build
	mkdir -p src/example/dist
	chmod -R 777 src/example/dist
	cp _build/default/src/ast_explorer_refmt.bc.js ./src/example/dist/ast_explorer_refmt.bc.js
	pnpm serve -p 3030 src/example

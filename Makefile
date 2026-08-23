.DEFAULT_GOAL := all
.NOTPARALLEL:

ROOT := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
TONGUE_DIR ?= $(ROOT)/tongue
VSCODE_DIR := $(ROOT)/vscode
WRIT_DIR := $(ROOT)/writ
PREFIX ?= $(HOME)/.local
BIN_DIR ?= $(PREFIX)/bin
VSIX := $(VSCODE_DIR)/dist/tung-vscode.vsix
LANGUAGE_NAMES := $(VSCODE_DIR)/generated/language-names.json

CABAL ?= cabal
NPM ?= npm
CODE ?= code
RUNGHC ?= runghc
BENCH_RUNS ?= 3
BENCH_SIZE ?= 1000

.PHONY: all setup build test benchmark docs install \
	bookhoard-membership compiler-build compiler-test compiler-install runner-check \
	language-metadata \
	vscode-deps vscode-build vscode-test extension-package extension-install \
	writ-deps writ-build writ-test

all: build

setup: test docs install
	@printf 'ready: tung runner, vscode extension, and documentation\n'

build: compiler-build vscode-build writ-build

test: compiler-test vscode-test writ-test

benchmark: compiler-build
	@printf 'benchmarking tung compiler and evaluator\n'
	@cd "$(TONGUE_DIR)" && $(CABAL) bench tung-benchmark --benchmark-options="--runs $(BENCH_RUNS) --size $(BENCH_SIZE)"

install: compiler-install extension-install

bookhoard-membership:
	@cd "$(TONGUE_DIR)" && $(RUNGHC) -isrc tool/bookhoard-membership.hs

compiler-build: bookhoard-membership
	@printf 'building tung compiler\n'
	@cd "$(TONGUE_DIR)" && $(CABAL) build exe:tung

compiler-test: language-metadata
	@printf 'testing tung compiler\n'
	@cd "$(TONGUE_DIR)" && $(CABAL) test all

compiler-install: compiler-build
	@printf 'installing tung runner at %s\n' "$(BIN_DIR)/tung"
	@mkdir -p "$(BIN_DIR)"
	@install -m 755 "$$(cd "$(TONGUE_DIR)" && $(CABAL) list-bin exe:tung)" "$(BIN_DIR)/tung"
	@$(MAKE) --no-print-directory runner-check

runner-check:
	@printf 'checking installed tung runner\n'
	@printf '%s\n' 'let value identity = value' | (cd /tmp && "$(BIN_DIR)/tung" --check-stdin) | grep -qx 'type ok'
	@printf '%s\n' ' let value = 1 ' | (cd /tmp && "$(BIN_DIR)/tung" --format-stdin) | grep -qx 'let value = 1'
	@if ! printf '%s' ":$$PATH:" | grep -Fq ':$(BIN_DIR):'; then \
		printf 'add %s to path to run: tung filename.tung\n' "$(BIN_DIR)"; \
	fi

vscode-deps:
	@cd "$(VSCODE_DIR)" && $(NPM) install

language-metadata: compiler-build
	@printf 'generating editor language metadata with tung\n'
	@tmp="$(LANGUAGE_NAMES).tmp"; \
		cd "$(TONGUE_DIR)" && "$$( $(CABAL) list-bin exe:tung)" --run-quiet tool/language-names.tung > "$$tmp"; \
		if ! cmp -s "$$tmp" "$(LANGUAGE_NAMES)"; then mv "$$tmp" "$(LANGUAGE_NAMES)"; else rm -f "$$tmp"; fi

vscode-build: language-metadata vscode-deps
	@printf 'building vscode client and language server\n'
	@cd "$(VSCODE_DIR)" && $(NPM) run build

vscode-test: language-metadata vscode-deps
	@printf 'checking and testing vscode client and language server\n'
	@cd "$(VSCODE_DIR)" && $(NPM) run check
	@cd "$(VSCODE_DIR)" && $(NPM) test

extension-package: vscode-build
	@printf 'packaging vscode extension\n'
	@mkdir -p "$(dir $(VSIX))"
	@cd "$(VSCODE_DIR)" && $(NPM) exec -- vsce package --readme-path readme.adoc --allow-missing-repository --skip-license --out "$(VSIX)"

extension-install: extension-package
	@printf 'installing or updating vscode extension\n'
	@$(CODE) --install-extension "$(VSIX)" --force

writ-deps:
	@cd "$(WRIT_DIR)" && $(NPM) install

writ-build: language-metadata writ-deps
	@printf 'building documentation generator\n'
	@cd "$(WRIT_DIR)" && $(NPM) run build

writ-test: language-metadata writ-deps
	@printf 'checking and testing documentation generator\n'
	@cd "$(WRIT_DIR)" && $(NPM) run check
	@cd "$(WRIT_DIR)" && $(NPM) test

docs: language-metadata writ-deps
	@printf 'generating documentation\n'
	@cd "$(WRIT_DIR)" && $(NPM) run docs
	@printf 'documentation: %s\n' "$(WRIT_DIR)/bookhoard/index.html"

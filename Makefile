.DEFAULT_GOAL := all
.NOTPARALLEL:

ROOT := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
TONGUE_DIR ?= $(ROOT)/tongue
PREFIX ?= $(HOME)/.local
BIN_DIR ?= $(PREFIX)/bin
NPM_STAMP := $(ROOT)/node_modules/.tung-workspace-stamp

CABAL ?= cabal
NODE ?= node
NPM ?= npm
RUBY ?= ruby
CODE ?= code
BENCH_RUNS ?= 3
BENCH_SIZE ?= 1000

.PHONY: all setup build test benchmark install \
	compiler-build compiler-test compiler-install runner-check prose-test \
	editor-deps editor-build editor-test language-metadata extension-package extension-install formula-check

all: build

setup: test install
	@printf 'ready: tung runner and vscode extension\n'

build: compiler-build editor-build

test: prose-test formula-check compiler-test editor-test

benchmark: compiler-build
	@printf 'benchmarking tung compiler and evaluator\n'
	@cd "$(TONGUE_DIR)" && $(CABAL) bench tung-benchmark --benchmark-options="--runs $(BENCH_RUNS) --size $(BENCH_SIZE)"

install: compiler-install extension-install

compiler-build:
	@printf 'building tung compiler\n'
	@cd "$(TONGUE_DIR)" && $(CABAL) build exe:tung

compiler-test:
	@printf 'testing tung compiler\n'
	@cd "$(TONGUE_DIR)" && $(CABAL) test all

prose-test:
	@printf 'checking repository prose\n'
	@$(NODE) --test tool/prose.test.mjs

$(NPM_STAMP): package.json package-lock.json vscode/package.json
	@$(NPM) ci
	@touch "$@"

editor-deps: $(NPM_STAMP)

language-metadata: compiler-build editor-deps
	@TUNG_EXECUTABLE="$$(cd "$(TONGUE_DIR)" && $(CABAL) list-bin exe:tung)" $(NPM) run update:metadata --workspace=tung-vscode

editor-build: language-metadata
	@$(NPM) run build --workspace=tung-vscode

editor-test: compiler-build editor-deps
	@TUNG_EXECUTABLE="$$(cd "$(TONGUE_DIR)" && $(CABAL) list-bin exe:tung)" $(NPM) run test --workspace=tung-vscode

formula-check:
	@$(RUBY) -c Formula/tung.rb

extension-package: language-metadata
	@$(NPM) run package --workspace=tung-vscode

extension-install: extension-package
	@$(CODE) --install-extension "$(ROOT)/vscode/dist/tung-vscode.vsix" --force

compiler-install: compiler-build
	@printf 'installing tung runner at %s\n' "$(BIN_DIR)/tung"
	@mkdir -p "$(BIN_DIR)"
	@install -m 755 "$$(cd "$(TONGUE_DIR)" && $(CABAL) list-bin exe:tung)" "$(BIN_DIR)/tung"
	@$(MAKE) --no-print-directory runner-check

runner-check:
	@printf 'checking installed tung runner\n'
	@printf '%s\n' 'let value identity [] value' | (cd /tmp && "$(BIN_DIR)/tung" --check-stdin) | grep -qx 'type ok'
	@printf '%s\n' ' let value [] 1 ' | (cd /tmp && "$(BIN_DIR)/tung" --format-stdin) | grep -qx 'let value [] 1'
	@if ! printf '%s' ":$$PATH:" | grep -Fq ':$(BIN_DIR):'; then \
		printf 'add %s to path to run: tung filename.tung\n' "$(BIN_DIR)"; \
	fi

#
# © 2019 Tocqueville Group
#
# SPDX-License-Identifier: AGPL-3.0-or-later
#

.PHONY: dev test haddock haddock-no-deps stylish lint clean

# Options for development
STACK_DEV_OPTIONS = --fast --ghc-options -Wwarn --file-watch
# Options to build more stuff (tests and benchmarks)
STACK_BUILD_MORE_OPTIONS = --test --bench --no-run-tests --no-run-benchmarks
# Options for tests
STACK_DEV_TEST_OPTIONS = --fast
# Options passed to test executable
TEST_ARGUMENTS ?= ""

# Build everything (including tests and benchmarks) with development options.
dev:
	stack build $(STACK_DEV_OPTIONS) $(STACK_BUILD_MORE_OPTIONS) morley

# Run tests in all packages which have them.
test:
	stack test morley $(STACK_DEV_TEST_OPTIONS) \
		--test-arguments "--color $(TEST_ARGUMENTS)"

# Run haddock for all packages.
haddock:
	stack haddock morley

# Run haddock for all our packages, but not for dependencies.
haddock-no-deps:
	stack haddock morley --no-haddock-deps

stylish:
	stylish-haskell -i `find src -iname '*.hs'`

lint:
	scripts/lint.sh

clean:
	stack clean

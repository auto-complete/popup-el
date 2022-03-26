SHELL := /usr/bin/env bash

EMACS ?= emacs
EASK ?= eask

TEST-FILES := $(shell ls test/popup-*.el)

.PHONY: clean package install compile lint unix-test

ci: clean package install compile

clean:
	@echo "Cleaning..."
	$(EASK) clean-all

package:
	@echo "Packaging..."
	$(EASK) autoloads
	$(EASK) pkg-file
	$(EASK) package

install:
	@echo "Installing..."
	$(EASK) install

compile:
	@echo "Compiling..."
	$(EASK) compile

lint:
	@echo "Linting..."
	$(EASK) lint

unix-test:
	@echo "Testing..."
	$(EASK) exec ert-runner -L . $(LOAD-TEST-FILES) -t '!no-win' -t '!org'

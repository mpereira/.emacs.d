EMACS_VERSION := 26.2
EMACS := /Applications/Emacs-$(EMACS_VERSION).app/Contents/MacOS/Emacs

PROJECT_ROOT := $(shell pwd)
TEST_RUNS_DIRECTORY := $(PROJECT_ROOT)/test_runs
TEST_HOME := $(TEST_RUNS_DIRECTORY)/$(shell date +%Y%m%d%H%M%S)
TEST_HOME_EMACSD := $(TEST_HOME)/.emacs.d

CUSTOM_EL := $(PROJECT_ROOT)/custom.el
OLD_ELPA := $(PROJECT_ROOT)/$(shell date +%Y%m%d%H%M%S)

.DEFAULT_GOAL := test

.PHONY:        \
	clean        \
	recompile    \
	rm-custom.el \
	test-clean   \
	test-quick

clean-elc:
	find . -name "*.elc" -type f | xargs rm -f

recompile: clean
	$(EMACS) --batch --eval '(byte-recompile-directory "$(PROJECT_ROOT)" 0 t)'

test-quick:
	@$(EMACS) --debug-init 2>/dev/null

test:
	mkdir -p $(TEST_HOME_EMACSD)
	cp init.el $(TEST_HOME_EMACSD)
	cp configuration.org $(TEST_HOME_EMACSD)
	touch $(TEST_HOME_EMACSD)/custom.el
	cp org-gcal-secrets.el $(TEST_HOME_EMACSD)
	cp wolfram-secrets.el $(TEST_HOME_EMACSD)
	cp circe-secrets.el $(TEST_HOME_EMACSD)
	@echo "git diff:"
	git diff
	@echo
	@echo "Opening Emacs with \$$HOME:"
	tree -a $(TEST_HOME_EMACSD)
	HOME="$(TEST_HOME)" $(EMACS) --debug-init 2>/dev/null

rm-custom.el:
	rm -f $(CUSTOM_EL)

custom.el:
	touch $(CUSTOM_EL)

test-clean:
	rm -rf $(TEST_RUNS_DIRECTORY)/*

clean: test-clean rm-custom.el custom.el clean-elc

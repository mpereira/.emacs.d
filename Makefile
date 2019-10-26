EMACS_VERSION := 26.3
EMACS := /Applications/Emacs-$(EMACS_VERSION).app/Contents/MacOS/Emacs

PROJECT_ROOT := $(shell pwd)
TEST_RUNS_DIRECTORY := $(PROJECT_ROOT)/test_runs
TEST_HOME := $(TEST_RUNS_DIRECTORY)/$(shell date +%Y%m%d%H%M%S)
TEST_HOME_EMACSD := $(TEST_HOME)/.emacs.d

CUSTOM_EL := $(PROJECT_ROOT)/custom.el

TEST_ELPA_NAME := elpa-$(shell date +%Y%m%d%H%M%S)
TEST_ELPA_FROM_TEST_NAME := elpa-from-test-$(shell date +%Y%m%d%H%M%S)
TEST_ELPA := $(PROJECT_ROOT)/$(TEST_ELPA_NAME)
TEST_ELPA_FILE := .test-elpa

.DEFAULT_GOAL := test

.PHONY:        \
	clean        \
	recompile    \
	rm-custom.el \
	clean-test   \
	test-quick

clean-elc:
	find . -name "*.elc" -type f | xargs rm -f

recompile: clean
	$(EMACS) --batch --eval '(byte-recompile-directory "$(PROJECT_ROOT)" 0 t)'

# test:
# 	@echo "git diff:"
# 	@git diff
# 	@echo
# 	@echo $(TEST_ELPA_NAME) > $(TEST_ELPA_FILE)
# 	@mv elpa $(TEST_ELPA)
# 	$(EMACS) --debug-init 2>/dev/null
# 	@mv elpa $(TEST_ELPA_FROM_TEST_NAME)
# 	@mv $(TEST_ELPA) elpa

# Add condition: if TEST_ELPA_FILE exists and points to a file.
test-revert-elpa:
	@mv elpa $(shell cat $(TEST_ELPA_FILE))
	@mv $(shell cat $(TEST_ELPA_FILE)) elpa

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

clean-test:
	rm -rf $(TEST_RUNS_DIRECTORY)/*

clean: clean-test rm-custom.el custom.el clean-elc

# EMACS_VERSION := 26.3
# EMACS := /Applications/Emacs-$(EMACS_VERSION).app/Contents/MacOS/Emacs
EMACS := emacs

PROJECT_ROOT				:= $(shell pwd)
TEST_RUNS_DIRECTORY := $(PROJECT_ROOT)/test-runs
TEST_HOME						:= $(TEST_RUNS_DIRECTORY)/$(shell date +%Y%m%d%H%M%S)
TEST_HOME_EMACSD		:= $(TEST_HOME)/.emacs.d

TEST_ELPA									:= $(PROJECT_ROOT)/$(TEST_ELPA_NAME)
TEST_ELPA_FILE						:= .test-elpa
TEST_ELPA_FROM_TEST_NAME	:= elpa-from-test-$(shell date +%Y%m%d%H%M%S)
TEST_ELPA_NAME						:= elpa-$(shell date +%Y%m%d%H%M%S)

CUSTOM_EL := $(PROJECT_ROOT)/custom.el
ELPA_FILE := .elpa

BACKUP_DATE					:= $(shell date +%Y%m%d%H%M%S)
BACKUP_DIRECTORY		:= $(PROJECT_ROOT)/package-repository-backups
BACKUP_ELPA					:= $(BACKUP_DIRECTORY)/elpa-$(BACKUP_DATE)
BACKUP_ELPA_FILE		:= $(BACKUP_DIRECTORY)/.latest-backup-elpa
BACKUP_QUELPA				:= $(BACKUP_DIRECTORY)/quelpa-$(BACKUP_DATE)
BACKUP_QUELPA_FILE	:= $(BACKUP_DIRECTORY)/.latest-backup-quelpa

.DEFAULT_GOAL := test

.PHONY: setup
setup:
	bash setup.sh

.PHONY: byte-recompile
byte-recompile: clean-elc
	$(EMACS) --batch --eval '(byte-recompile-directory "$(PROJECT_ROOT)" 0 t)'

$(BACKUP_DIRECTORY):
	mkdir $(BACKUP_DIRECTORY)

.PHONY: backup-dependencies
backup-dependencies: $(BACKUP_DIRECTORY)
	echo $(BACKUP_ELPA) > $(BACKUP_ELPA_FILE)
	echo $(BACKUP_QUELPA) > $(BACKUP_QUELPA_FILE)
	rsync -a elpa $(BACKUP_ELPA)
	rsync -a quelpa $(BACKUP_QUELPA)

.PHONY: restore-latest-dependencies
restore-latest-dependencies: $(BACKUP_DIRECTORY)
	mv $(shell cat $(BACKUP_ELPA_FILE)) elpa
	mv $(shell cat $(BACKUP_QUELPA_FILE)) quelpa

.PHONY: show-backed-up-dependencies
show-backed-up-dependencies: $(BACKUP_DIRECTORY)
	ls $(shell cat $(BACKUP_ELPA_FILE))/elpa
	ls $(shell cat $(BACKUP_QUELPA_FILE))/quelpa/build

.PHONY: clean-backed-up-dependencies
clean-backed-up-dependencies: $(BACKUP_DIRECTORY)
	 rm -rf $(BACKUP_DIRECTORY)/*

# test:
# 	echo "git diff:"
# 	git diff
# 	echo
# 	echo $(TEST_ELPA_NAME) > $(TEST_ELPA_FILE)
# 	mv elpa $(TEST_ELPA)
# 	$(EMACS) --debug-init 2>/dev/null
# 	mv elpa $(TEST_ELPA_FROM_TEST_NAME)
# 	mv $(TEST_ELPA) elpa

# Add condition: if TEST_ELPA_FILE exists and points to a file.
.PHONY: test-revert-elpa
test-revert-elpa:
	mv elpa $(shell cat $(TEST_ELPA_FILE))
	mv $(shell cat $(TEST_ELPA_FILE)) elpa

.PHONY: test-quick
test-quick: clean-elc clean-custom.el clean-hidden-cache
	$(EMACS) --debug-init 2>/dev/null

.PHONY: test
test:
	mkdir -p $(TEST_HOME_EMACSD)
	cp init.el $(TEST_HOME_EMACSD)
	cp configuration.org $(TEST_HOME_EMACSD)
	touch $(TEST_HOME_EMACSD)/custom.el
	cp org-gcal-secrets.el $(TEST_HOME_EMACSD)
	cp wolfram-secrets.el $(TEST_HOME_EMACSD)
	cp circe-secrets.el $(TEST_HOME_EMACSD)
	echo "git diff:"
	git diff
	echo
	echo "Opening Emacs with \$$HOME:"
	tree -a $(TEST_HOME_EMACSD)
	HOME="$(TEST_HOME)" $(EMACS) --debug-init 2>/dev/null

custom.el:
	touch $(CUSTOM_EL)

.PHONY: clean-custom.el
clean-custom.el:
	rm -f $(CUSTOM_EL)

.PHONY: clean-elc
clean-elc:
	find . -name "*.elc" -type f | xargs rm -f

.PHONY: clean-test
clean-test:
	rm -rf $(TEST_RUNS_DIRECTORY)/*

.PHONY: clean-elpy
clean-elpy:
	rm -rf elpy

.PHONY: clean-emojis
clean-emojis:
	rm -rf emojis

.PHONY: clean
clean: clean-test clean-custom.el custom.el clean-elc clean-elpy clean-emojis

.PHONY: clean-elpa
clean-elpa:
	rm -rf elpa

.PHONY: clean-quelpa
clean-quelpa:
	rm -rf quelpa

.PHONY: clean-hidden-cache
clean-hidden-cache:
	rm -rf .cache

.PHONY: clean-dependencies
clean-dependencies: clean-elpa clean-quelpa

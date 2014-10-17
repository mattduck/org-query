#!/bin/sh
emacs -batch -l ert -l org-query.el -l tests.el -f ert-run-tests-batch-and-exit

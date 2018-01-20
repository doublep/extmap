#! /usr/bin/env bash

set -e

if [ -z "$EMACS" ] ; then
    EMACS=emacs
fi

if [ -z "$ERT_SELECTOR" ] ; then
    ERT_SELECTOR=t
fi

$EMACS -batch                                                           \
       --eval "(message \"Using Emacs %s\" (emacs-version))"            \
       -l extmap.el                                                     \
       -l test/extmap-test.el                                           \
       --eval "(let ((ert-quiet t)) (ert-run-tests-batch-and-exit (quote ${ERT_SELECTOR})))"

$EMACS -Q --batch                                                       \
       --eval "(setq byte-compile-error-on-warn t)"                     \
       --eval "(batch-byte-compile)" extmap.el

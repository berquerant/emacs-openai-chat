#!/bin/bash

d=$(cd $(dirname $0)/..; pwd)

sitelispd="${d}/site-lisp"
mkdir -p "$sitelispd"
git clone https://github.com/tkf/emacs-request.git --branch v0.3.2 "${sitelispd}/emacs-request" --depth 1
git clone https://github.com/magnars/s.el.git --branch 1.13.0 "${sitelispd}/s" --depth 1

run-emacs() {
    emacs --batch --quick --directory . \
          --eval "(add-to-list 'load-path \"${sitelispd}/emacs-request\")" \
          --eval "(add-to-list 'load-path \"${sitelispd}/s\")" \
          "$@"
}

set -ex

run-emacs --eval "(progn (setq byte-compile-error-on-warn t) (batch-byte-compile))" *.el
run-emacs --load tests/*.el --eval "(ert-run-tests-batch-and-exit t)"

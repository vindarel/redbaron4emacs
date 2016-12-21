;;; red4emacs-tests.el --- Tests for red4emacs.el

;; Copyright (C) 2016  Free Software Foundation, Inc.

;; Author: vindarel

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;

;;; Code:

(require 'ert)
(require 'dash)

(load-file "red4emacs.el")
(require 'red4e)

(setq red4emacs-tests-input-one-liner "def foo(self, one, *args, **kwargs): pass")
(setq red4emacs-tests-input-one-liner--argslist '("self" "one" "*args" "**kwargs"))

(setq red4e-tests-input-multiline "def foo(self,
one,
*args,
**kwargs): pass")

;; Read args
(ert-deftest test-get-args-one-liner ()
  (with-temp-buffer
    (insert red4emacs-tests-input-one-liner)
    (should (equal (red4e--get-function-args)
                   red4emacs-tests-input-one-liner--argslist))))

(ert-deftest test-get-args-multiline ()
  (with-temp-buffer
    (insert red4e-tests-input-multiline)
    (should (equal (red4e--get-function-args)
                   red4emacs-tests-input-one-liner--argslist))))

(ert-deftest test-get-args-noargs ()
  (with-temp-buffer
    (insert "def foo(): pass")
    (should (equal (red4e--get-function-args)
                   '("")))))

;; Add arg
(ert-deftest test-add-arg-multiline ()
  (with-temp-buffer
    (insert red4e-tests-input-multiline)
    (python-mode)
    (red4e-add-arg "three=three()")
    (should (equal (red4e--get-function-args)
                   (-insert-at 2 "three=three()" red4emacs-tests-input-one-liner--argslist)))
    (should (equal nil
                   (red4e-args-on-one-line-p)))))

;; Add a decorator
(ert-deftest red4e-test-decorator-add ()
  (with-temp-buffer
    (insert "\n") ;; or we're at beginning of buffer: xxx
    (insert red4e-tests-input-multiline)
    (python-mode)
    (red4e--decorator-add "decotest")
    (should (equal (red4e--decorator-read)
                   "@decotest"))))

;; Remove a decorator
(ert-deftest red4e-test-decorator-remove ()
  (with-temp-buffer
    (insert "\n")
    (insert "
@one
@two
def foo():
    pass
")
    (python-mode)
    (should (equal (red4e--decorator-remove)
                   "@two"))))

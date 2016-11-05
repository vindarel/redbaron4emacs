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
(load-file "red4emacs.el")
(require 'red4e)

(setq red4emacs-tests-input-one-liner "def foo(self, one, *args, **kwargs): pass")
(setq red4emacs-tests-input-one-liner--argslist '("self" "one" "*args" "**kwargs"))

(ert-deftest test-get-args-one-liner ()
  (with-temp-buffer
    (insert red4emacs-tests-input-one-liner)
    (should (equal (red4e--get-function-args)
                   red4emacs-tests-input-one-liner--argslist))))

(ert-deftest test-get-args-multiline ()
  (with-temp-buffer
    (insert "def foo(self,
one,
*args,
**kwargs): pass")
    (should (equal (red4e--get-function-args)
                   red4emacs-tests-input-one-liner--argslist))))

(ert-deftest test-get-args-noargs ()
  (with-temp-buffer
    (insert "def foo(): pass")
    (should (equal (red4e--get-function-args)
                   '("")))))

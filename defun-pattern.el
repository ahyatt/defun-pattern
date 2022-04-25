;;; defun-pattern.el --- A way to define methods that have multiple pattern-style arglists and bodies.

;; Copyright (c) 2022  Free Software Foundation, Inc.

;; Author: Andrew Hyatt <ahyatt@gmail.com>
;; Homepage: https://github.com/ahyatt/defun-pattern
;; Keywords: Pattern, Matching 
;; Version: 0.1
;; Package-Requires: ((cl-lib "0.5"))
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This code provides a way to have a defun that, instead of having a single
;; arglist, has many, which are pattern-matched against. Each arglist then has
;; its own corresponding body.
;;
;; The implementation is based on pcase, and works by transforming the arglists
;; and bodies into a corresponding backquoted pcase.
;;
;; For usage, see `defun-pattern' documentation.
;;
;; 

;;; Code:

(require 'pcase)
(require 'cl-extra)

(defun defun-pattern-unquote (elem)
  "If ELEM is a inserted element, return uninserted version of ELEM.
Otherwise, if it isn't inserted, return ELEM."
  (pcase elem
    ((and (pred listp)
         ls
         (guard (= 2 (length ls)))
         (guard (equal 'quote (car ls)))) (cadr ls))
    (e e)))

(defconst defun-pattern-known-funcs
  '(rx or and let guard app pred cl-type seq)
  "All functions that need to be unquoted in a backquoted pcase.")

(defun defun-pattern-needs-comma (elem)
  "Return t if ELEM is a classic pcase pattern."
  ;; Lists include quoted elements and functions.
  (pcase elem
    ((and (pred listp) ls)
     (member (car ls) defun-pattern-known-funcs))
    ((pred symbolp) t)))

(defun defun-pattern-correct-quote (pattern)
  "Correct quoting for our backquote style.
Argument PATTERN is a pattern found in an `defun-pattern' arglist."
  (pcase pattern
    ((pred defun-pattern-needs-comma)
     (list '\, pattern))
    ((pred null)
     nil)
    ((pred listp)
     (mapcar #'defun-pattern-correct-quote
             (defun-pattern-unquote pattern)))
    (_ pattern)))

(defun defun-pattern-to-pcase (pattern)
  "Transform a `defun-pattern' matcher to a pcase matcher.
Argument PATTERN is an arglist that needs to be transformed."
  (pcase pattern
    ((pred listp) (list '\` (defun-pattern-correct-quote pattern)))
    (_ pattern)))

(defun defun-pattern-normal-args (arglist)
  "Return all pre-&rest args in arglist."
  (cl-subseq arglist 0 (cl-position '&rest arglist)))

(defun defun-pattern-rest-arg (arglist)
  "Return the &rest arg in arglist, as a 1-element list."
  (when (eq (nth (- (length arglist) 2) arglist) '&rest)
    (last arglist)))

(def-edebug-elem-spec 'defun-pattern-patterns
  '((sexp def-body)))

(def-edebug-spec defun-pattern
  (&define name lambda-list lambda-doc &rest defun-pattern-patterns))

;;;###autoload
(defmacro defun-pattern (func arglist &rest sub-defs)
  "Define a function which uses pattern matching to execute.
The macro takes an arglist, which is mostly for documentation
purposes. The patterns have their own pattern/arglists to match
against, which then become the arglist for the corresponding
body function.

If nothing matches, `nil' is returned.

The patterns are basically the same as backquoted pcase, except
for quoting differences to make it more similar to normal
function arglists. The following differences exist from
backquoted pcase:

 1. The list is not backquoted.
 2. Nothing needs to be prefixed with a comma.
 3. Symbols, when they don't represent variables, but rather
 constants to match against, need to be quoted.

Think of the patterns as arglists, except some of the arguments
are symbols which represent variables which are bound, and others
are pcase matchers, which can be constants or special matchers
such as `pred' and others.

Example of a simple defun-pattern:
(defun-pattern fibonacci (n)
 \"Compute the fibonacci sequence in a naïve way.\"
 ((0) 0)
 ((1) 1)
 ((n) (+ (fibonacci (- n 1))
    (fibonacci (- n 2)))))

Using an arglist `(&rest args)' will operate as normal in elisp,
with ARGS being a list. That means patterns matching it have to
assume it is a list. If you want to dispense with this containing
list, but still have a variable number of arguments, you can use
the special arglist (&rest _).

Using a normal arglist of &rest it would be:
(defun-pattern fibonacci (&rest args)
 \"Compute the fibonacci sequence in a naïve way.\"
 (((0)) 0)
 (((1)) 1)
 (((n)) (+ (fibonacci (- n 1))
    (fibonacci (- n 2)))))

Using the special arglist of (&rest _), this becomes:
(defun-pattern fibonacci (&rest _)
 \"Compute the fibonacci sequence in a naïve way.\"
 ((0) 0)
 ((1) 1)
 ((n) (+ (fibonacci (- n 1))
    (fibonacci (- n 2)))))

However, arguments passed in can be used in body methods, and it
is often useful to have real arguments so that documentation and
other standard elisp tools can function well.

Further examples can be found in the file defun-pattern-test.el.
"
  (declare (doc-string 3)
           (indent 2))
  `(defun ,func ,arglist
     (pcase
         ,@(let* ((normal-args (defun-pattern-normal-args arglist))
                  (rest-arg (defun-pattern-rest-arg arglist))
                  (num-args (+ (length normal-args) (length rest-arg))))
             (cond ((and (= num-args 1) (equal (car rest-arg) '_))
                    rest-arg)
                   ((>= num-args 1)
                    (list (append '(list) normal-args rest-arg)))
                   (t (error "There must be at least one argument to match against!"))))
       ,@(mapcar (lambda (d)
                   (list (defun-pattern-to-pcase (car d))
                         (if (> (length (cdr d)) 1)
                             (append '(progn) (cdr d))
                           (cadr d))))
                 ;; Declaring the doc-string doesn't seem to actually remove the
                 ;; doc-string from the arg list, so we have to filter it out
                 ;; here.
                 (if (stringp (car sub-defs))
                     (cdr sub-defs)
                   sub-defs)))))

(provide 'defun-pattern)

;;; defun-pattern.el ends here

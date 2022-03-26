;;; defun-pattern.el --- A way to define methods that have multiple pattern-style arglists and bodies.

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
;; CAUTION: as of now, there is no ability to edebug the defun.

;;; Code:

(require 'pcase)

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
  (list '\` (defun-pattern-correct-quote pattern)))

(def-edebug-elem-spec 'defun-pattern-patterns
  '((sexp def-body)))

(def-edebug-spec defun-pattern
  (&define name [&optional stringp] &rest defun-pattern-patterns))

;;;###autoload
(defmacro defun-pattern (func &rest sub-defs)
  "Define a function with multiple arglists that are matched.
Each arglist is paired with a body function.

All arglists are based of the backquoted pcase form.

If nothing matches, `nil' is returned.

The patterns are basically the same as backquoted pcase, except
for quoting differences to make it more similar to normal
function arglists. The following differences exist from
backquoted pcase:

 1. The list is not backquoted.
 2. Nothing needs to be prefixed with a comma.
 3. Symbols, when they don't represent variables, but rather
 constants to match against, need to be quoted.

Example:
(defun-pattern fibonacci
 \"Compute the fibonacci sequence.\"
 ((0) 0)
 ((1) 1)
 ((n)
 (+ (fibonacci (- n 1))
    (fibonacci (- n 2)))))

Further examples can be found in defun-pattern-test.el.
"
  (declare (doc-string 2)
           (indent 2))
  `(defun ,func (&rest args)
     (pcase args
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

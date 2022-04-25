;; -*- lexical-binding: t; -*-
(require 'ert)

(ert-deftest defun-pattern-test-constants-and-variables ()
  (defun-pattern fibonacci (n)
    "Compute the fibonacci sequence."
    ((0) 0)
    ((1) 1)
    ((n)
     (+ (fibonacci (- n 1))
        (fibonacci (- n 2)))))
  (should (= (fibonacci 0) 0))
  (should (= (fibonacci 1) 1))
  (should (= (fibonacci 2) 1))
  (should (= (fibonacci 3) 2))
  (should (= (fibonacci 8) 21)))

(ert-deftest defun-pattern-test-mixed ()
  (defun-pattern crit (roll attributes)
    "D&D 5e critical hit computation, taking in the results of a d20
and a list of attributes currently in effect."
    ((20 _) t)
    ((19 (pred (member 'improved-crit))) t))
  (should-not (crit 1 '(improved-crit)))
  (should-not (crit 19 '(other-attribute)))
  (should (crit 19 '(improved-crit)))
  (should (crit 20 nil)))

(ert-deftest defun-pattern-test-repeated ()
  (defun-pattern repeatedp (&rest _)
    "Test for repeated pattern, returns nil, 'once, or 'twice, or
'split."
    ((a a) 'once)
    ((a a a) 'twice)
    ((a b a) 'split))
  (should-not (repeatedp 3))
  (should-not (repeatedp 3 1 1))
  (should-not (repeatedp 3 3 1))
  (should (eq 'once (repeatedp 3 3)))
  (should (eq 'once (repeatedp 'z 'z)))
  (should (eq 'twice (repeatedp 3 3 3)))
  (should (eq 'split (repeatedp 3 'z 3))))

(ert-deftest defun-pattern-test-destruct ()
  (defun-pattern middle-eq-plus-one (&rest _)
    "Test for destructuring patterns."
    (((_ a _) b) (= (+ 1 a) b)))
  (should-not (middle-eq-plus-one '(3) 3))
  (should-not (middle-eq-plus-one '(z) 'z))
  (should-not (middle-eq-plus-one '(1 3 9) 3))
  (should (middle-eq-plus-one '(1 3 9) 4)))

(ert-deftest defun-pattern-ordinary-arglist ()
  (defun-pattern fibonacci-with-rest-args (&rest args)
    "Compute the fibonacci sequence."
    (((0)) 0)
    (((1)) 1)
    (((n))
     (+ (fibonacci-with-rest-args (- n 1))
        (fibonacci-with-rest-args (- n 2)))))
  (should (= (fibonacci-with-rest-args 8) 21)))

(ert-deftest defun-pattern-arglist-required ()
  (should-error (defun-pattern my-bad-defun () (+ 1 1))))

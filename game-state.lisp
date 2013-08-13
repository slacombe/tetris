(defpackage :tetris
  (:use :cl :asdf)
  (:export :start))

(in-package :tetris)

(defparameter *states* '(game-over removing-lines advancing))
(defparameter *state* 'advancing)

(defun change-state (new-state)
  (setf *state* new-state))

(defpackage tetris-asd
  (:use :cl :asdf))

(require :lispbuilder-sdl)

(in-package :tetris-asd)

(defsystem tetris
  :description "Transfer Manager"
  :version "1.0"
  :author "Sylvain Lacombe"
  :components ((:file "game-state")
	       (:file "tetris" :depends-on ("game-state"))
	       (:file "board" :depends-on ("game-state"))
	       (:file "figures" :depends-on ("game-state"))))

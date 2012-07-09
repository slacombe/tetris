(defpackage tetris-system
  (:use :cl :asdf))

(require :lispbuilder-sdl)

(in-package :tetris-system)

(defsystem tetris
  :description "Transfer Manager"
  :version "1.0"
  :author "Sylvain Lacombe"
  :components ((:file "tetris")
	       (:file "board")
	       (:file "figures")))

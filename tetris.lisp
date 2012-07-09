(defpackage :tetris
  (:use :cl))

(in-package :tetris)

;; Size of each tetris block
(defconstant block-size 22)

(defun start ()
  (sdl:with-init ()
    (sdl:window 400 600 :title-caption "CL-Tetris")
    (setf (sdl:frame-rate) 60)
      
    (let ((board (make-instance 'tetris-board :x 5 :y 5))
	  (count 0)
	  (accel nil))
      (new-game board)
      (sdl:with-events ()
	(:quit-event () t)
	(:key-down-event (:key key) 
	 (when (sdl:key= key :sdl-key-escape)
	   (sdl:push-quit-event))
	 (when (sdl:key= key :sdl-key-up)
	   (try-rotate-figure board))
	 (when (sdl:key= key :sdl-key-left)
	   (move-figure board -1 0))
	 (when (sdl:key= key :sdl-key-right)
	   (move-figure board 1 0))
	 (when (sdl:key= key :sdl-key-down)
	   (setf accel t)))
	(:key-up-event (:key key)
	 (when (sdl:key= key :sdl-key-down)
	   (setf accel nil)))
	
	(:idle () 
         (sdl:clear-display sdl:*black*)

	 (incf count)
	 (when (or (zerop (mod count 120)) accel)
	   (do-step board))
	
	 (draw board)
	 
	 (sdl:update-display))))))

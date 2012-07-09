(in-package :tetris)

;; Figures 
(defparameter *figures*
  (list
   (make-array 4 :initial-contents #((-1 . 0) ( 0 . 0) (-1 . 1) (0 . 1)))
   (make-array 4 :initial-contents #((-1 . 1) (-1 . 0) ( 0 . 0) (1 . 0)))
   (make-array 4 :initial-contents #((-1 . 0) ( 0 . 0) ( 1 . 0) (0 . 1)))
   (make-array 4 :initial-contents #((-2 . 0) (-1 . 0) (0 . 0) (1 . 0)))
   (make-array 4 :initial-contents #((-1 . 0) (0 . 0) (1 . 0) (1 . 1)))
   (make-array 4 :initial-contents #((-1 . 1) (0 . 1) (0 . 0) (1 . 0)))
   (make-array 4 :initial-contents #((-1 . 0) (0 . 0) (0 . 1) (1 . 1)))))

(defclass figure ()
  ((x :accessor figure-x :initarg :x)
   (y :accessor figure-y :initarg :y)
   (color :reader color :initform nil)
   (blocks :reader blocks :initform nil)))

(defun draw-figure (board figure surface)
  "Draw the figure on the tetris board at current position"
  (let ((color (slot-value figure 'color)))
    (loop
       for block across (slot-value figure 'blocks)
       for rel-x = (car block)
       for rel-y = (cdr block)
       for x = (* (+ rel-x (figure-x figure)) block-size)
       for y = (* (+ rel-y (figure-y figure)) block-size)
       do (sdl:draw-surface-at-* (elt (blocks board) color) x y 
				 :surface surface))))

(defun advance-figure (figure)
  (incf (slot-value figure 'y)))

(defun create-random-figure (x y)
  (let ((f (make-instance 'figure :x x :y y))
	(c (random (length *figures*))))
    (setf (slot-value f 'color) c)
    (setf (slot-value f 'blocks) (copy-figure (elt *figures* c)))
    f))

(defun make-move (figure x y)
  (setf (figure-x figure) x
	(figure-y figure) y))

(defun rotate-figure (figure)
  (loop 
     for block across (slot-value figure 'blocks)
     for x = (car block)
     for y = (cdr block)
     do (setf (car block) y
	      (cdr block) (* x -1))))

(defun copy-figure (blocks)
  (let ((a (make-array 4)))
    (loop
       for b across blocks
       for i from 0 to 3
       do (setf (aref a i) (copy-list (aref blocks i))))
    a))
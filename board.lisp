(in-package :tetris)

(defun load-block (filename)
  (sdl:load-image filename))

(defun make-blocks ()
  "Create the graphic blocks"
  (list
   (load-block "red.bmp")
   (load-block "blue.bmp")
   (load-block "yellow.bmp")
   (load-block "orange.bmp")
   (load-block "purple.bmp")
   (load-block "light-blue.bmp")
   (load-block "green.bmp")))

(defclass tetris-board ()
  ((x :reader board-x :initarg :x)
   (y :reader board-y :initarg :y)
   (width :reader board-width :initform 10)
   (height :reader board-height :initform 18)
   (board :initform nil)
   (surface :reader surface :initform nil)
   (next-surface :reader next-surface :initform nil)
   (blocks :accessor blocks :initform nil)
   (current-figure :accessor current-figure :initform nil)
   (next-figure :accessor next-figure :initform nil)))

(defmethod initialize-instance :after ((b tetris-board) &key)
  (let ((width (board-width b))
	(height (board-height b)))

    (setf (blocks b) (make-blocks))
    
    (setf (slot-value b 'surface)
	  (sdl:create-surface (* block-size width)
			      (* block-size height) 
			      :x 5 :y 5))

    (setf (slot-value b 'next-surface)
	  (sdl:create-surface (* block-size 6)
			      (* block-size 4)
			      :x (* block-size 11)
			      :y (* block-size 1)))))

(defmethod draw ((board tetris-board))
  (let ((width (* (board-width board) block-size))
	(height (* (board-height board) block-size)))
    (sdl:fill-surface sdl:*black* :surface (surface board))
    (sdl:draw-rectangle-* (- (board-x board) 1) (- (board-y board) 1)
			  (+ width 2) (+ height  2))
    (draw-figure board (current-figure board) (surface board))
    (draw-board board)
    (sdl:blit-surface (surface board))
    (sdl:blit-surface (next-surface board))))

(defmethod choose-next ((board tetris-board))
  (let ((figure (create-random-figure 4 0)))
    (setf (slot-value board 'next-figure) figure)
    (sdl:fill-surface sdl:*black* :surface (surface board))
    (sdl:fill-surface sdl:*black* :surface (next-surface board))
    (draw-figure board figure (next-surface board))))

(defmethod new-game ((board tetris-board))
  "Start a new game"
  (change-state 'advancing)
  (setf (slot-value board 'board)
	(make-array (list (board-width board) (board-height board))
		    :initial-element nil))
  (choose-next board)
  (setf (current-figure board) (next-figure board))
  (choose-next board)
  (setf (aref (slot-value board 'board) 9 17) 0))

(defmethod draw-board ((b tetris-board))
  "Draw the tetris board"
  (let ((board (slot-value b 'board)))
    (loop
       for y from 0 to (- (board-height b) 1)
       do (loop
	     for x from 0 to (- (board-width b) 1)
	     for bl = (aref board x y)
	     do (when bl (draw-block b x y bl))))))

(defmethod draw-block ((b tetris-board) x y block)
  "Draw a tetris block"
  (let ((gx (* x block-size))
	(gy (* y block-size))
	(color (elt (slot-value b 'blocks) block)))
    (sdl:draw-surface-at-* color gx gy :surface (surface b))))

(defun do-step (board)
  "Advance the game of a new step"
  (case *state*
    (advancing
     (if (try-advance-figure board)
	 (advance-figure (current-figure board))
	 (if (< (figure-y (current-figure board)) 1)
	     (change-state 'game-over)
	     (progn 
	       (fix-figure board)
	       (let ((lines (lines-to-erase board)))
		 (if lines
		     (erase-lines board lines)))
	       (choose-next board)))))
    (removing-lines
     (change-state 'advancing))
    (game-over
     (play-game-over board))))

(defun fix-figure (board)
  "The figure is fix to is final position on the board"
  (let* ((figure (current-figure board))
	 (fx (figure-x figure))
	 (fy (figure-y figure))
	 (b (slot-value board 'board)))
    (loop 
       for block across (blocks figure)
       for x = (+ fx (car block))
       for y = (+ fy (cdr block))
       do (setf (aref b x y) (color figure)))
    (setf (current-figure board) (next-figure board))))

(defun test-move (board dir-x dir-y)
  "Check if the current figure can be move to the desired position"
  (let ((blocks (slot-value (current-figure board) 'blocks)))
    (loop 
       for block across blocks
       for x = (+ (car block) dir-x)
       for y = (+ (cdr block) dir-y)
       always (empty-square board x y))))

(defun test-rotate (board)
  "Check if the current figure can be rotated"
  (let ((blocks (slot-value (current-figure board) 'blocks))
	(f (current-figure board)))
    (loop
       for block across blocks
       for x = (car block)
       for y = (cdr block)
       for test-x = (+ (figure-x f) y)
       for test-y = (+ (figure-y f) x)
       always (empty-square board test-x test-y))))

(defun empty-square (tb x y)
  "Check if a square is empty"
  (let ((board (slot-value tb 'board)))
    (or (< y 0)
	(and (>= x 0) 
	     (< x (board-width tb))
	     (< y (board-height tb))
	     (null (aref board x y))))))

(defun move-figure (board dir-x dir-y)
  "Move the figure to its next position"
  (let* ((f (current-figure board))
	 (x (figure-x f))
	 (y (figure-y f))
	 (new-x (+ x dir-x))
	 (new-y (+ y dir-y)))
  (if (test-move board new-x new-y)
      (make-move f new-x new-y))))

(defun try-rotate-figure (board)
  "Try to rotate the figure to see if can be done"
  (let ((f (current-figure board)))
    (if (test-rotate board)
	(rotate-figure f))))

(defun try-advance-figure (board)
  "Check if the figure can advance a line"
  (let* ((f (current-figure board))
	 (fx (figure-x f))
	 (fy (+ (figure-y f) 1)))
    (loop
       for block across (slot-value f 'blocks)
       for x = (car block)
       for y = (cdr block)
       for new-x = (+ x fx)
       for new-y = (+ y fy)
       always (empty-square board new-x new-y))))

(defun lines-to-erase (tb)
  "Returns the full lines to erase"
  (let ((board (slot-value tb 'board))
	(full-lines nil))
    (loop
       for y from (1- (board-height tb)) downto 0
       do (if (loop
		 for x from 0 to (1- (board-width tb))
		 always (aref board x y))
	      (push y full-lines)))
    full-lines))

(defun erase-lines (tb lines)
  "Erase the full lines"
  (declare (optimize (debug 1)))
  (loop
     for line in (sort lines #'<)
     do (erase-line tb line)))

(defun erase-line (tb line)
  "Erase a full line and lower the rest of the board"
  (let ((board (slot-value tb 'board)))
    (loop
       for l from line downto 1
       do (loop
	     for x from 0 to (1- (board-width tb))
	     do (setf (aref board x l) 
		      (aref board x (1- l)))))))

(defun play-game-over (board)
  (sdl:draw-box-* 5 5 (* 10 block-size) (* 2 block-size) 
		  :surface (surface board)))

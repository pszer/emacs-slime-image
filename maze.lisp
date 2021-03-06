(defconstant +north-flag+ #x01)
(defconstant +east-flag+  #x02)
(defconstant +south-flag+ #x04)
(defconstant +west-flag+  #x08)
(defconstant +visited-flag+ #x10)

(defun has-flag (value flag)
  (not (= 0 (logand value flag))))
(defmacro north-flag-p (value) `(has-flag ,value +north-flag+))
(defmacro east-flag-p (value) `(has-flag ,value +east-flag+))
(defmacro south-flag-p (value) `(has-flag ,value +south-flag+))
(defmacro west-flag-p (value) `(has-flag ,value +west-flag+))
(defmacro visited-p (value) `(has-flag ,value +visited-flag+))

(defun opposite-flag (flag)
  (case flag
    ((#x01) +south-flag+)
    ((#x04) +north-flag+)
    ((#x02) +west-flag+)
    ((#x08) +east-flag+)))

(defun make-grid (w h)
  (list* (make-array (* w h) :element-type '(unsigned-byte 5) :initial-element 0) w h))
(defun grid-cells  (grid) (car grid))
(defun grid-width  (grid) (cadr grid))
(defun grid-height (grid) (cddr grid))

(defmacro grid-op (name (grid x y &rest rest) out-of-bound-expr in-bound-expr)
  `(defun ,name (,grid ,x ,y ,@rest)
     (let ((w (grid-width  ,grid))
	   (h (grid-height ,grid)))
       (if (or (< ,x 0) (< ,y 0) (>= ,x w) (>= ,y h))
	   ,out-of-bound-expr
	   ,in-bound-expr))))
(grid-op grid-get (grid x y) nil (elt (grid-cells grid) (+ x (* y w))))
(grid-op grid-set (grid x y val) nil (setf (elt (grid-cells grid) (+ x (* y w))) val))
(grid-op grid-add-flag (grid x y flag) nil (setf (elt (grid-cells grid) (+ x (* y w))) (logior flag (elt (grid-cells grid) (+ x (* y w))))))

(defun make-maze (w h &key (start (cons 0 0)) (end (cons (1- w) (1- h))))
  (let ((grid (make-grid w h))
	(size (* w h))
	(visited-count 0))
    (labels ((iter (stack)
	       (let ((accessible nil))
		 (macrolet ((check (dx dy flag)
			      `(let* ((x (+ ,dx (caar stack)))
				      (y (+ ,dy (cdar stack)))
				      (c (grid-get grid x y)))
				 (when (and c (not (visited-p c)))
				   (setf accessible (cons (list x y ,flag) accessible))))))
		   (when (and (< visited-count size) stack)
		     (unless (visited-p (grid-get grid (caar stack) (cdar stack)))
		       (incf visited-count))
		     (grid-add-flag grid (caar stack) (cdar stack) +visited-flag+)
		     (check +0 -1 +north-flag+)
		     (check +1 +0 +east-flag+)
		     (check +0 +1 +south-flag+)
		     (check -1 +0 +west-flag+)
		     (if accessible
			 (let ((chosen (nth (random (length accessible)) accessible)))
			   (grid-add-flag grid (caar stack) (cdar stack) (third chosen))
			   (grid-add-flag grid (first chosen) (second chosen) (opposite-flag (third chosen)))
			   (iter (cons (cons (first chosen) (second chosen)) stack)))
			 (iter (cdr stack)))))))
	     (make-gap (pos)
	       (macrolet ((do-flag (flag) `(grid-add-flag grid (car pos) (cdr pos) ,flag)))
		 (cond ((= 0 (cdr pos)) (do-flag +north-flag+))
		       ((= 0 (car pos)) (do-flag +west-flag+))
		       ((= (1- h) (cdr pos)) (do-flag +south-flag+))
		       ((= (1- w) (car pos)) (do-flag +east-flag+))
		       (t (error "start/end not at edge of maze"))))))
      (iter (list start))
      (make-gap start)
      (make-gap end))
    grid))

(defun print-maze (maze)
  (loop for y from 0 to (1- (grid-height maze)) do
       (progn (loop for x from 0 to (1- (grid-width maze)) do
		   (format t "~d " (grid-get maze x y)))
	      (format t "~%"))))
(defun image-maze (maze size &optional (canvas (make-solid 255 255 255
							   (1+ (* size (grid-width maze)))
							   (1+ (* size (grid-height maze))))))
  (loop for y from 0 to (1- (grid-height maze)) do
       (loop for x from 0 to (1- (grid-width maze)) do
	    (macrolet ((wall (x1 y1 x2 y2 val flag)
			 `(unless (has-flag ,val ,flag)
			    (image:line canvas (+ pix-x ,x1) (+ pix-y ,y1) (+ pix-x ,x2) (+ pix-y ,y2)
					0 0 0))))
	      (let ((pix-x (* size x))
		    (pix-y (* size y))
		    (val (grid-get maze x y)))
		(wall 0 0 size 0 val +north-flag+)
		(wall 0 0 0 size val +west-flag+)
		(wall 0 size size size val +south-flag+)
		(wall size 0 size size val +east-flag+)))))
  canvas)

(defun display-maze (w h size)
  (display-image (image-maze (make-maze w h) size)))

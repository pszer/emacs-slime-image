(defun generate-function-values (fn x zoom w)
  "Generates a list of function values of fn for w test points from x-zoom to x+zoom."
  (let* ((half-w (floor w 2))
	 (x-step (float (/ zoom half-w)))
	 (x-start (- x zoom)))
    (labels ((get-values (x-pos x-pix result)
	       (if (> x-pix w)
		   result
		   (get-values (+ x-pos x-step)
			       (1+ x-pix)
			       (cons (handler-case (funcall fn x-pos)
				       (division-by-zero () :nan))
				     result)))))
      (nreverse (get-values (float x-start) 0 '())))))

(defun graph-values (values h &key (y-scale 1) (y-offset 0) (log-scale nil) (align :center) (colour '(0 0 0 0.5)) (canvas nil canvas-p))
  "Draws a list of numeric values as a graph on an image canvas."
    (labels ((t-scale (val)
	       (if log-scale
		   (* y-scale
		      (if (minusp val) -1 1) ; correct sign
		      (log (+ (abs val) 1) log-scale))
		   (* val y-scale)))
	     (t-align (val)
	       (let ((scaled-val (- (t-scale val) (* y-offset y-scale))))
		 (case align
		   ((:center) (- (floor h 2) scaled-val))
		   ((:bottom) (- h scaled-val))
		   ((:top)    (- scaled-val))
		   (otherwise (error "Bad alignment")))))
	     (t-val (val)
	       (if (eq val :nan)
		   :nan
		   (floor (t-align val))))
	     (in-range-p (val)
	       (and (>= val 0) (< val h)))
	     (clamp (x min max)
	       (if (< x min)
		   min
		   (if (> x max)
		       max
		       x))))
      (let ((t-values (mapcar #'t-val values))
	    (surface (if canvas-p canvas (make-solid 255 255 255 (length values) h)))
	    (r (first colour))
	    (g (second colour))
	    (b (third colour))
	    (a (fourth colour))
	    (low-clamp 0)
	    (max-clamp (+ h 0)))
	(loop
	   for x1 from 0
	   for x2 from 1
	   for v1 in t-values
	   for v2 in (cdr t-values)
	   do (unless (or (eq v1 :nan) (eq v2 :nan) (and (not (in-range-p v1)) (not (in-range-p v2))))
		(image:line surface
			    x1 (clamp v1 low-clamp max-clamp)
			    x2 (clamp v2 low-clamp max-clamp) r g b a)))
	surface)))

(defun in-range (x low upper)
  (and (>= x low) (<= x upper)))

(defun graph-unit-pix (w h zoom)
  (* (floor h 2) (/ w h) (/ 1 zoom)))

(defun graph-axes (x y zoom w h canvas &key (align :center) (colour '(0 0 0 1.0)) &allow-other-keys)
  "Draws the x and y axes for some given view parameters."
  (let* ((w-mid (floor w 2))
	 (h-mid (floor h 2))
	 (unit-pix (graph-x-unit-pix w h zoom))
	 (x-axis-y (floor (* y unit-pix)))
	 (y-axis-x (floor (+ (* (- x) unit-pix) w-mid)))
	 (r (first colour))
	 (g (second colour))
	 (b (third colour))
	 (a (fourth colour)))
    (case align
      ((:center) (incf x-axis-y h-mid))
      ((:bottom) (incf x-axis-y h))
      ((:top) nil))
    (when (in-range x-axis-y 0 h)
      (image:line canvas 0 x-axis-y w x-axis-y r g b a))
    (when (in-range y-axis-x 0 w)
      (image:line canvas y-axis-x 0 y-axis-x h r g b a))))

(defun graph-function (fn x y zoom w h &rest rest)
  "Returns an image of a graph of a function."
  (let ((canvas
	 (apply #'graph-values (list* (generate-function-values fn x zoom w)
				      h
				      :y-scale (graph-unit-pix w h zoom)
				      :y-offset y
				      rest))))
    (apply #'graph-axes (list* x y zoom w h canvas rest))))

(defun graph-functions (fns x y zoom w h &optional colours &rest rest)
  "Returns an image of several graphs drawn on top of each other."
  (let ((canvas (make-solid 255 255 255 w h))
	(cols (if colours
		  colours
		  (loop for fn in fns collect (list 0 0 0 0.5)))))
    (loop for fn  in fns
	  for col in cols do
	 (apply #'graph-function (list* fn x y zoom w h :canvas canvas :colour col rest)))
    canvas))

(defun display-graph (&rest args)
  "Displays a graph to a SLIME buffer."
  (display-image (apply #'graph-function args)))

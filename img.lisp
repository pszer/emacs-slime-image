(defun display-image-on-disk (filename)
  "Displays an image found on disk."
  (swank:eval-in-emacs `(slime-media-insert-image (create-image ,filename) ,filename)))
(defun display-image (image)
  "Displays an image made using the image library"
  (let ((temp-fname (format nil "/tmp/emacs-img-~d.gif" (get-universal-time))))
    (image:export-to-gif image temp-fname)
    (display-image-on-disk temp-fname)
    (swank:eval-in-emacs `(delete-file ,temp-fname))))
(defun display-image-and-store (image filename)
  (image:export-to-gif image filename)
  (display-image-on-disk filename))

(defun solid-background (image r g b)
  (image:rect image 0 0 (image:width image) (image:height image)
	      t r g b)
  image)
(defun make-solid (r g b w h)
  (solid-background (image:make-image w h) r g b))

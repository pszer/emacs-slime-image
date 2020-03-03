;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; (setq 'slime-enable-evaluate-in-emacs t)

(ql:quickload :image)

(defpackage #:emacs-img-asd
  (:use :cl :asdf :image)
  (:export
   :display-image-on-disk
   :display-image
   :solid-background
   :make-solid))
(in-package :emacs-img-asd)

(defsystem emacs-img
  :name "emacs-img"
  :version "0.0.0"
  :description "Emacs Images"
  :long-description "Procedures for displaying images and rendering primitives within an Emacs SLIME buffer."
  :serial t
  :components ((:file "img")
	       (:file "graph"))
  :depends-on (:image))

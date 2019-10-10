;;;; image-map.lisp 
;;
;; Copyright (c) 2019 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(in-package :image-map)

(defclass mapping ()
  ((bottom-left :initform (complex -1.0 -1.0) :initarg :bottom-left)
   (top-right :initform (complex 1.0 1.0) :initarg :top-right)
   (width :initform 100 :initarg :width)
   (height :initform 100 :initarg :height))
  (:documentation "A mapping between image coordinates and the complex plane."))

(defun set-pixel-png (img x y r g b)
  "Set a pixel in im at location x,y to color (r,g,b)."
  (setf (aref img x y 0) r)
  (setf (aref img x y 1) g)
  (setf (aref img x y 2) b))

(defun get-pixel-png (img x y)
  "Return pixel value at location x,y."
  (values (aref img x y 0)
          (aref img x y 1)
          (aref img x y 2)))

(defun transform (&key (in-file-name) (out-file-name))
  (let* ((img (png:decode-file in-file-name))
         (new-img (png:copy-image img)))
    (png:encode-file new-img out-file-name)))

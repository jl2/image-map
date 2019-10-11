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
   (width :initarg :width)
   (height :initarg :height)
   (r-diff :initarg :r-diff)
   (i-diff :initarg :i-diff))
  (:documentation "A mapping between image coordinates and the complex plane."))

(defun create-mapping (&key
                         (bottom-left (complex -1.0 -1.0))
                         (top-right (complex 1.0 1.0))
                         (width)
                         (height))
  (make-instance 'mapping
                 :bottom-left bottom-left
                 :top-right top-right
                 :width width
                 :height height
                 :r-diff (/ (- (realpart top-right) (realpart bottom-left)) width 1.0)
                 :i-diff (/ (- (imagpart top-right) (imagpart bottom-left)) height)))

(defun image-to-complex (i j map)
  (with-slots (bottom-left top-right width height r-diff i-diff) map
    (+ bottom-left (complex (* i r-diff)
                            (* (- height j) i-diff)))))

(defun complex-to-image (pt map)
  (with-slots (bottom-left top-right width height r-diff i-diff) map
    (let ((offset (- pt bottom-left)))
      (values (* (realpart offset) r-diff width) (* (imagpart offset) i-diff height)))))

(defun complex-to-complex (pt map)
  (multiple-value-bind (i j) (complex-to-image pt map)
    (image-to-complex i j map)))

(defun image-to-image (i j map)
  (complex-to-image
   (image-to-complex i j map) map))

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

(defun transform (&key
                    (function #'identity)
                    (bottom-left (complex -1.0 -1.0))
                    (top-right (complex 1.0 1.0))
                    (in-file-name)
                    (out-file-name))
  (declare (ignorable function))
  (let* ((img (png:decode-file in-file-name))
         (new-img (png:copy-image img))
         (map (make-instance 'mapping
                             :width (png:image-width img)
                             :height (png:image-height img)
                             :bottom-left bottom-left
                             :top-right top-right)))
    (declare (ignorable map))
    (ensure-directories-exist out-file-name)
    (png:encode-file new-img out-file-name)))

(defun generate-log-grid-png (output-file-name &key
                                                 (width 512)
                                                 (height 512)
                                                 (x-multiplier 2)
                                                 (y-multiplier 2)
                                                 (red 255)
                                                 (green 255)
                                                 (blue 255))
  (let* ((img (png:make-image height width 3 8)))
    (loop
       for x = 1 then (* x-multiplier x)
       while (<= x width) do
         (dotimes (y height)
           (set-pixel-png img (floor (1- x)) y red green blue)))
    (loop
       for y = 1 then (* y-multiplier y)
       while (<= y height) do
         (dotimes (x width)
           (set-pixel-png img x (floor (1- y)) red green blue)))
    (ensure-directories-exist output-file-name)
    (png:encode-file img output-file-name)))

(defun generate-grid-png (output-file-name &key
                                             (width 512)
                                             (height 512)
                                             (x-count 32)
                                             (y-count 32)
                                             (red 255)
                                             (green 255)
                                             (blue 255))
  (let* ((img (png:make-image height width 3 8))
         (dx (/ (1- width) x-count 1.0))
         (dy (/ (1- height) y-count 1.0)))
    (loop
       for x = 0 then (+ x dx)
       while (< x width) do
         (dotimes (y height)
           (set-pixel-png img (floor x) y red green blue)))
    (loop
       for y = 0 then (+ y dy)
       while (< y height) do
         (dotimes (x width)
           (set-pixel-png img x (floor y) red green blue)))
    (ensure-directories-exist output-file-name)
    (png:encode-file img output-file-name)))

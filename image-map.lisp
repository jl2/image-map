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
  ((top-left :initform (complex -1.0 -1.0) :initarg :top-left)
   (bottom-right :initform (complex 1.0 1.0) :initarg :bottom-right)
   (width :initarg :width)
   (height :initarg :height)
   (r-size :initarg :r-size)
   (i-size :initarg :i-size)
   (r-diff :initarg :r-diff)
   (i-diff :initarg :i-diff))
  (:documentation "A mapping between image coordinates and the complex plane."))

(defun create-mapping (&key
                         (top-left (complex -1.0 -1.0))
                         (bottom-right (complex 1.0 1.0))
                         (width)
                         (height))
  (make-instance 'mapping
                 :top-left top-left
                 :bottom-right bottom-right
                 :width width
                 :height height
                 :r-size (- (realpart bottom-right) (realpart top-left))
                 :i-size (- (imagpart bottom-right) (imagpart top-left))
                 :r-diff (/ (- (realpart bottom-right) (realpart top-left)) width 1.0)
                 :i-diff (/ (- (imagpart bottom-right) (imagpart top-left)) height 1.0)))

(defun image-to-complex (i j map)
  (with-slots (top-left bottom-right width height r-diff i-diff) map
    (+ top-left (complex (* i r-diff)
                         (* j i-diff)))))

(defun complex-to-image (pt map)
  (with-slots (top-left bottom-right width height r-size i-size) map
    (let ((offset (- (complex (realpart pt) (imagpart pt)) top-left )))
      (values (round (* (/ (realpart offset) r-size) width))
              (round (* (/ (imagpart offset) i-size) height))))))

(defun complex-to-complex (pt map)
  (multiple-value-bind (i j) (complex-to-image pt map)
    (image-to-complex i j map)))

(defun image-to-image (i j map)
  (complex-to-image
   (image-to-complex i j map) map))

(defun same-pixel (pt1 pt2 map)
  (multiple-value-bind (i1 j1) (complex-to-image pt1 map)
    (multiple-value-bind (i2 j2) (complex-to-image pt2 map)
      (and (= i1 i2) (= j1 j2)))))

(defun set-pixel-png (img x y r g b)
  "Set a pixel in im at location x,y to color (r,g,b)."
  (when (and (>= x 0) (>= y 0)
             (< x (png:image-width img))
             (< y (png:image-height img)))
    (setf (aref img y x 0) r)
    (setf (aref img y x 1) g)
    (setf (aref img y x 2) b)))

(defun get-pixel-png (img x y)
  "Return pixel value at location x,y."
  (if (and (>= x 0) (>= y 0)
           (< x (png:image-width img))
           (< y (png:image-height img)))
      (values (aref img y x 0)
              (aref img y x 1)
              (aref img y x 2))
      (values 0 0 0)))

(defun transform (&key
                    (function #'identity)
                    (top-left (complex -1.0 1.0))
                    (bottom-right (complex 1.0 -1.0))
                    (in-file-name)
                    (out-file-name))
  (declare (ignorable function))
  (let* ((img (png:decode-file in-file-name))
         (width (png:image-width img))
         (height (png:image-height img))
         (new-img (png:make-image height width 3))
         (map (create-mapping
               :width width
               :height height
               :top-left top-left
               :bottom-right bottom-right)))
    (dotimes (i width)
      (dotimes (j height)
        (let* ((pt (image-to-complex i j map))
               (new-pt (funcall function pt)))
          (multiple-value-bind (new-i new-j) (complex-to-image new-pt map)
            (multiple-value-bind (r g b) (get-pixel-png img new-i new-j)
              ;; (format t "i ~a j ~a pt ~a color ~a ~a ~a new-pt ~a new-i ~a new-j ~a~%" i j pt r g b new-pt new-i new-j)
              (set-pixel-png new-img i j r g b))))))
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

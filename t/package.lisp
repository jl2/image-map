;; package.lisp
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

(in-package :cl-user)
(defpackage :image-map.test
  (:use :cl
        :fiveam
        :alexandria
        :image-map))

(in-package :image-map.test)

(def-suite :image-map)
(in-suite :image-map)

(test identity-transform
 (let ((in-file-name (format nil "~a~a"
                      (asdf:system-source-directory :image-map.test)
                      "t/examples/triangles.png")))
   (uiop:with-temporary-file (:pathname out-file-name :keep t)
     (format t
             "~%Writing identity transform image to ~a~%"
             out-file-name)

     (is-true (transform
               :function #'identity
               :in-file-name in-file-name
               :out-file-name out-file-name))

     (let* ((inf (osicat-posix:stat in-file-name))
            (outf (osicat-posix:stat out-file-name)))
       ;; Should probably do an image diff or something, but this is good enough for now :-/
       (is (= (osicat-posix:stat-size inf) (osicat-posix:stat-size outf)))))))

(test coordinate-conversions
  (let ((map (create-mapping :top-left (complex -1.0 1.0)
                             :bottom-right (complex 1.0 -1.0)
                             :width 100
                             :height 100)))
    (loop for (i j) on '(75 82
                         54 34
                         71 94
                         99 54
                         80 37
                         87 19
                         15 16
                         67 78
                         47 66
                         97 85)
       by #'cddr
       do
         (multiple-value-bind (new-i new-j) (image-to-image i j map)
           (is (= i new-i))
           (is (= j new-j))))

    (dolist (pt '(#C(-0.07311033278174417 -0.40901662191991717)
                  #C(0.80369449329458 -0.8119645720101363)
                  #C(0.1852823024844823 -0.021227601174207855)
                  #C(-0.24417913079456177 -0.2528985738602074)
                  #C(-0.2659561429776729 0.4739386442751914)
                  #C(-0.4975246128672324 0.12091001374270816)
                  #C(-0.6754963313697115 0.7175435228288656)
                  #C(0.10285053942484135 0.947168179635975)
                  #C(0.7434339306215563 0.7871356120870519)
                  #C(0.22539731609709834 -0.9659664276316606)))
      
      (let ((new-pt (complex-to-complex pt map)))
        (is (same-pixel pt new-pt map))))

    ;; Test a few important values
    (is (= 
         (complex -1.0 1.0)
         (image-to-complex 0 0 map)))
    (is (= (complex -1.0 -1.0)
           (image-to-complex 0 100 map)))
    (is (= 
         (complex 1.0 -1.0)
         (image-to-complex 100 100 map)))
    (is (= 
         (complex 1.0 1.0)
         (image-to-complex 100 0 map)))
    (is (= 
         (complex 1.0 1.0)
         (image-to-complex 100 0 map)))
    (multiple-value-bind (i j) (complex-to-image (complex 0.0 0.0) map)
      (is (= 50 i))
      (is (= 50 j)))))

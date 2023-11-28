(in-package #:lv.jonis.fmt.tests)

(define-test all)

(define-test nbytes
  :parent all
  (is equal "1000B" (format nil "~/fmt:nbytes/" 1000))
  (is equal "1.0K" (format nil "~/fmt:nbytes/" 1024))
  (is equal "1.0k" (format nil "~:/fmt:nbytes/" 1000))
  ;; TODO: Not sure if we want rounding up :/
  (skip "Not sure about rounding"
        (is equal "1.9K" (format nil "~/fmt:nbytes/" 2000)))
  (is equal "977K" (format nil "~/fmt:nbytes/" (expt 10 6)))
  (is equal "1.0M" (format nil "~/fmt:nbytes/" (expt 2 20)))
  (is equal "1.0M" (format nil "~:/fmt:nbytes/" (expt 10 6))))

(define-test bytes
  :parent all
  (let* ((vector #(#x4d #x5a #x90 #x00 #x03))
         (list (coerce vector 'list)))
    (is equal "4D5A900003" (format nil "~/fmt:bytes/" vector))
    (is equal "4D5A900003" (format nil "~/fmt:bytes/" list))))

(define-test timestamp
  :parent all
  (let ((2k-utc (encode-universal-time 0 0 0 1 1 2000 0)))
    (is eql #\space (schar (format nil "~,' /fmt:ts/" 2k-utc) 10))
    (is equal "2000-01-01T00:00:00+00:00" (format nil "~0/fmt:ts/" 2k-utc))
    (is equal "2000-01-01T00:00:00Z" (format nil "~'Z/fmt:ts/" 2k-utc))
    (is equal "2000-01-01T00:00:00" (format nil "~0@/fmt:ts/" 2k-utc))
    (is equal "2000-01-01T00:00:00" (format nil "~'Z@/fmt:ts/" 2k-utc))
    (is equal "1999-12-31T22:00:00+02:00" (format nil "~+2/fmt:ts/" 2k-utc))
    (is equal "2000-01-01T02:00:00-02:00" (format nil "~-2/fmt:ts/" 2k-utc))))

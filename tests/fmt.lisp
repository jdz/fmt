(in-package #:lv.jonis.fmt.tests)

(define-test all)

(define-test nbytes
  :parent all
  ;; TODO: Not sure if we want rounding up :/
  (skip "Not sure about rounding"
        (is equal "1.9K" (format nil "~/fmt:nbytes/" 2000)))
  (is equal "976.6K" (format nil "~/fmt:nbytes/" (expt 1000 2)))
  )

(define-test nbytes-dec
  :parent nbytes
  (is equal "999"  (format nil "~:/fmt:nbytes/" 999))
  (is equal "1.0k" (format nil "~:/fmt:nbytes/" (expt 1000 1)))
  (is equal "1.0M" (format nil "~:/fmt:nbytes/" (expt 1000 2)))
  (is equal "1.0G" (format nil "~:/fmt:nbytes/" (expt 1000 3)))
  (is equal "1.0T" (format nil "~:/fmt:nbytes/" (expt 1000 4)))
  (is equal "1.0P" (format nil "~:/fmt:nbytes/" (expt 1000 5)))
  (is equal "1.0E" (format nil "~:/fmt:nbytes/" (expt 1000 6)))
  (is equal "1.0Z" (format nil "~:/fmt:nbytes/" (expt 1000 7)))
  (is equal "1.0Y" (format nil "~:/fmt:nbytes/" (expt 1000 8)))
  (is equal "1.0R" (format nil "~:/fmt:nbytes/" (expt 1000 9)))
  (is equal "1.0Q" (format nil "~:/fmt:nbytes/" (expt 1000 10)))
  (is equal "1000.0Q" (format nil "~:/fmt:nbytes/" (expt 1000 11))))

(define-test nbytes-dec-rounded
  :parent nbytes
  (is equal "2.9M" (format nil "~:@/fmt:nbytes/" 2949999))
  (is equal "3.0M" (format nil "~:@/fmt:nbytes/" 2950000)))

(define-test nbytes-bin
  :parent nbytes
  (is equal "1023" (format nil "~/fmt:nbytes/" 1023))
  (is equal "1.0K" (format nil "~/fmt:nbytes/" 1024))
  (is equal "1.0M" (format nil "~/fmt:nbytes/" (expt 1024 2)))
  (is equal "1.0G" (format nil "~/fmt:nbytes/" (expt 1024 3)))
  (is equal "1.0T" (format nil "~/fmt:nbytes/" (expt 1024 4)))
  (is equal "1.0P" (format nil "~/fmt:nbytes/" (expt 1024 5)))
  (is equal "1.0E" (format nil "~/fmt:nbytes/" (expt 1024 6)))
  (is equal "1.0Z" (format nil "~/fmt:nbytes/" (expt 1024 7)))
  (is equal "1.0Y" (format nil "~/fmt:nbytes/" (expt 1024 8)))
  (is equal "1024.0Y" (format nil "~/fmt:nbytes/" (expt 1024 9))))

(define-test nbytes-bin-round
  :parent nbytes
  (is equal "2.9M" (format nil "~/fmt:nbytes/" 3093299))
  (is equal "3.0M" (format nil "~/fmt:nbytes/" 3093300)))

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

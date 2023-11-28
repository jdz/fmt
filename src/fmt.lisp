(in-package #:lv.jonis.fmt)

(defun nbytes (stream n &optional colon? at-sign?)
  "Formats amount of N bytes in a human-readable fashion using powers
of 1024, or powers of 1000 with colon modifier."
  ;; XXX: Do we really need to use double-floats?
  ;;
  ;; TODO:
  ;; - We can use LOG, at least for the metric units.
  (declare (ignore at-sign?)
           (type unsigned-byte n))
  (cond ((zerop n)
         (write-string "0B" stream))
        (t
         (multiple-value-bind (base units)
             (if colon?
                 (values 1000.0d0 "BkMGTPEZY")
                 (values 1024.0d0 "BKMGTPEZY"))
           (loop for i fixnum from 0 below (1- (length units))
                 for f double-float = (coerce n 'double-float) then (/ f base)
                 until (< f base)
                 finally (let ((unit (schar units i)))
                           (if (and (< f 10) (plusp i))
                               (format stream "~,1F~A" f unit)
                               (format stream "~D~A" (round f) unit))))))))

(defun bytes (stream bytes &optional colon? at-sign?)
  "Formats a sequence of BYTES as hex-digit pairs."
  (declare (ignore colon? at-sign?))
  (etypecase bytes
    (vector
     (loop for byte of-type (unsigned-byte 8) across bytes
           do (when (< byte #x10)
                (write-char #\0 stream))
              (write byte :stream stream
                          :base 16
                          :radix nil
                          :readably nil
                          :escape nil)))
    (list
     (format stream "~{~2,'0x~}" bytes))))

(defun to-cl-time-zone (datum)
  (case datum
    ((#\Z #\z) 0)
    (otherwise datum)))

(defun ts (stream time &optional colon? at-sign? tz (separator #\T))
  (declare (ignore colon?))
  "Formats a universal time as an ISO 8601 time string.  Use the at-sign
modifier to not include timezone information."
  ;; TODO:
  ;;
  ;; - Specify date/time separator.
  ;; - Allow to only include date or time.
  ;; - Include nanoseconds if TIME is a float.
  ;; - Use colon to format unix time?
  ;;
  ;; XXX: Maybe instead of trying to put all the options into this
  ;; function, we should add a separate one (named `tsz' maybe) to
  ;; format with the timezone?
  (multiple-value-bind (tz zulu)
      (case tz
        ((#\Z #\z)
         (values 0 t))
        (otherwise
         (values tz nil)))
    (multiple-value-bind (second minute hour date month year dow daylight? zone)
        (decode-universal-time time (to-cl-time-zone tz))
      (declare (ignore dow daylight?))
      (format stream "~D-~2,'0D-~2,'0D~C~2,'0D:~2,'0D:~2,'0D"
              year month date separator hour minute second)
      (unless at-sign?
        (cond (zulu
               (assert (zerop zone))
               (write-char #\Z stream))
              ((not (integerp zone))
               (error "Don't know how to format timezone: ~S" zone))
              ((minusp zone)
               (format stream "-~2,'0D:00" (abs zone)))
              (t
               (format stream "+~2,'0D:00" zone)))))))
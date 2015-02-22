(add-to-load-path (string-append (dirname (dirname (current-filename)))))

(define-module (tests test-guile-beagleio)
  #:use-module (srfi srfi-64)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 rdelim)
  #:use-module (beagleio))

(test-begin "gpio")

(define (gpio-sysfs-path pin-number)
  (string-append "/sys/class/gpio/gpio" (number->string pin-number)))

(define (sysfs-exists? pin)
  (access? (gpio-sysfs-path pin) F_OK))

(test-eq 53  (gpio-number-lookup "USR0"))
(test-eq 54  (gpio-number-lookup "USR1"))
(test-eq 55  (gpio-number-lookup "USR2"))
(test-eq 56  (gpio-number-lookup "USR3"))
(test-eq 0   (gpio-number-lookup "P8_1"))
(test-eq 0   (gpio-number-lookup "P8_2"))
(test-eq 38  (gpio-number-lookup "P8_3"))
(test-eq 39  (gpio-number-lookup "P8_4"))
(test-eq 34  (gpio-number-lookup "P8_5"))
(test-eq 35  (gpio-number-lookup "P8_6"))
(test-eq 66  (gpio-number-lookup "P8_7"))
(test-eq 67  (gpio-number-lookup "P8_8"))
(test-eq 69  (gpio-number-lookup "P8_9"))
(test-eq 68  (gpio-number-lookup "P8_10"))
(test-eq 45  (gpio-number-lookup "P8_11"))
(test-eq 44  (gpio-number-lookup "P8_12"))
(test-eq 23  (gpio-number-lookup "P8_13"))
(test-eq 26  (gpio-number-lookup "P8_14"))
(test-eq 47  (gpio-number-lookup "P8_15"))
(test-eq 46  (gpio-number-lookup "P8_16"))
(test-eq 27  (gpio-number-lookup "P8_17"))
(test-eq 65  (gpio-number-lookup "P8_18"))
(test-eq 22  (gpio-number-lookup "P8_19"))
(test-eq 63  (gpio-number-lookup "P8_20"))
(test-eq 62  (gpio-number-lookup "P8_21"))
(test-eq 37  (gpio-number-lookup "P8_22"))
(test-eq 36  (gpio-number-lookup "P8_23"))
(test-eq 33  (gpio-number-lookup "P8_24"))
(test-eq 32  (gpio-number-lookup "P8_25"))
(test-eq 61  (gpio-number-lookup "P8_26"))
(test-eq 86  (gpio-number-lookup "P8_27"))
(test-eq 88  (gpio-number-lookup "P8_28"))
(test-eq 87  (gpio-number-lookup "P8_29"))
(test-eq 89  (gpio-number-lookup "P8_30"))
(test-eq 10  (gpio-number-lookup "P8_31"))
(test-eq 11  (gpio-number-lookup "P8_32"))
(test-eq 9   (gpio-number-lookup "P8_33"))
(test-eq 81  (gpio-number-lookup "P8_34"))
(test-eq 8   (gpio-number-lookup "P8_35"))
(test-eq 80  (gpio-number-lookup "P8_36"))
(test-eq 78  (gpio-number-lookup "P8_37"))
(test-eq 79  (gpio-number-lookup "P8_38"))
(test-eq 76  (gpio-number-lookup "P8_39"))
(test-eq 77  (gpio-number-lookup "P8_40"))
(test-eq 74  (gpio-number-lookup "P8_41"))
(test-eq 75  (gpio-number-lookup "P8_42"))
(test-eq 72  (gpio-number-lookup "P8_43"))
(test-eq 73  (gpio-number-lookup "P8_44"))
(test-eq 70  (gpio-number-lookup "P8_45"))
(test-eq 71  (gpio-number-lookup "P8_46"))
(test-eq 0   (gpio-number-lookup "P9_1"))
(test-eq 0   (gpio-number-lookup "P9_2"))
(test-eq 0  (gpio-number-lookup "P9_3"))
(test-eq 0   (gpio-number-lookup "P9_4"))
(test-eq 0   (gpio-number-lookup "P9_5"))
(test-eq 0   (gpio-number-lookup "P9_6"))
(test-eq 0   (gpio-number-lookup "P9_7"))
(test-eq 0   (gpio-number-lookup "P9_8"))
(test-eq 0   (gpio-number-lookup "P9_9"))
(test-eq 0   (gpio-number-lookup "P9_10"))
(test-eq 30  (gpio-number-lookup "P9_11"))
(test-eq 60  (gpio-number-lookup "P9_12"))
(test-eq 31  (gpio-number-lookup "P9_13"))
(test-eq 50  (gpio-number-lookup "P9_14"))
(test-eq 48  (gpio-number-lookup "P9_15"))
(test-eq 51  (gpio-number-lookup "P9_16"))
(test-eq 5   (gpio-number-lookup "P9_17"))
(test-eq 4   (gpio-number-lookup "P9_18"))
(test-eq 13  (gpio-number-lookup "P9_19"))
(test-eq 12  (gpio-number-lookup "P9_20"))
(test-eq 3   (gpio-number-lookup "P9_21"))
(test-eq 2   (gpio-number-lookup "P9_22"))
(test-eq 49  (gpio-number-lookup "P9_23"))
(test-eq 15  (gpio-number-lookup "P9_24"))
(test-eq 117 (gpio-number-lookup "P9_25"))
(test-eq 14  (gpio-number-lookup "P9_26"))
(test-eq 115 (gpio-number-lookup "P9_27"))
(test-eq 113 (gpio-number-lookup "P9_28"))
(test-eq 111 (gpio-number-lookup "P9_29"))
(test-eq 112 (gpio-number-lookup "P9_30"))
(test-eq 110 (gpio-number-lookup "P9_31"))
(test-eq 0   (gpio-number-lookup "P9_32"))
(test-eq 0   (gpio-number-lookup "P9_33"))
(test-eq 0   (gpio-number-lookup "P9_34"))
(test-eq 0   (gpio-number-lookup "P9_35"))
(test-eq 0   (gpio-number-lookup "P9_36"))
(test-eq 0   (gpio-number-lookup "P9_37"))
(test-eq 0   (gpio-number-lookup "P9_38"))
(test-eq 0   (gpio-number-lookup "P9_39"))
(test-eq 0   (gpio-number-lookup "P9_40"))
(test-eq 20  (gpio-number-lookup "P9_41"))
(test-eq 7   (gpio-number-lookup "P9_42"))
(test-eq 0   (gpio-number-lookup "P9_43"))
(test-eq 0   (gpio-number-lookup "P9_44"))
(test-eq 0   (gpio-number-lookup "P9_45"))
(test-eq 0   (gpio-number-lookup "P9_46"))



(test-error 'gpio-error (gpio-setup "non existing name"))

(define-syntax test-gpio-sysfs-export
  (syntax-rules ()
    ((_ (proc name) ...)
     (test-group
      "testing gpio-setup export to sysfs"
      (test-assert
       (let ((pin (gpio-number-lookup name)))
	 (apply proc (list name))
	 (sysfs-exists? pin)
	  #t)) ...))))

(test-group-with-cleanup
 "returns a gpio connection"
 (test-assert (gpio? (gpio-setup "P8_3")))
 (test-assert (gpio? (gpio-setup "P8_4")))
 (test-assert (gpio? (gpio-setup "P8_5")))
 (test-assert (gpio? (gpio-setup "P8_6")))
 (test-assert (gpio? (gpio-setup "P9_14")))
 (test-assert (gpio? (gpio-setup "P9_16")))
 (test-gpio-sysfs-export
  (gpio-setup "P8_3")
  (gpio-setup "P8_4")
  (gpio-setup "P8_5")
  (gpio-setup "P8_6")
  (gpio-setup "P9_14")
  (gpio-setup "P9_16"))
 (gpio-cleanup-all))

(test-assert (number? INPUT))
(test-assert (number? OUTPUT))
(test-assert (not (equal? INPUT OUTPUT)))

(test-group
 "setting the gpio direction"
 (test-group-with-cleanup
  (test-equal
   "in"
   (let ((gpio (gpio-setup "P8_3")))
     (gpio-direction-set! gpio INPUT)
     (call-with-input-file
	 (string-append (gpio-sysfs-path (gpio-number-lookup "P8_3")) "/direction")
       (lambda (port)
	 (read-line port)))))
  (gpio-cleanup-all))
 (test-group-with-cleanup
  (test-equal
   "out"
   (let ((gpio (gpio-setup "P8_3")))
     (gpio-direction-set! gpio OUTPUT)
     (call-with-input-file
	 (string-append (gpio-sysfs-path (gpio-number-lookup "P8_3")) "/direction")
       (lambda (port)
	 (read-line port)))))
  (gpio-cleanup-all))
 (test-group-with-cleanup
  "returns a gpio object"
  (test-assert
   (let ((gpio (gpio-setup "P8_3")))
     (gpio? (gpio-direction-set! gpio OUTPUT))))
  (test-assert
   (let ((gpio (gpio-setup "P8_3")))
     (gpio? (gpio-direction-set! gpio INPUT))))
  (gpio-cleanup-all)))

(test-group-with-cleanup
 "returns the correct boolean when testing if value is a gpio connection"
 (test-assert
  (not (gpio? 199)))
 (test-assert
  (gpio? (gpio-setup "P8_3")))
 (gpio-cleanup-all))

(test-group-with-cleanup
 "getting the gpio direction"
 (test-equal
  OUTPUT
  (let pio((gpio (gpio-setup "P8_3")))
    (gpio-direction-set! gpio OUTPUT)
    (gpio-direction gpio)))
 (test-equal
  INPUT
  (let ((gpio (gpio-setup "P8_3")))
    (gpio-direction-set! gpio INPUT)
    (gpio-direction gpio)))
 (gpio-cleanup-all))

(test-group
 "cleanup exports"
 (test-assert
  (let* ((names '("P8_3" "P9_16"))
	 (pins (map gpio-number-lookup names)))
    (map (lambda (name) (gpio-setup name)) names)
    (gpio-cleanup-all)
    (not (member #t (map sysfs-exists? pins))))))

(test-group
 "possible-gpio-levels"
 (test-assert (number? HIGH))
 (test-assert (number? LOW))
 (test-assert (not (eq? HIGH LOW))))

(test-group
 "setting the value of a gpio"
 (test-group-with-cleanup
  "returns a gpio connection"
  (test-assert
   (let ((gpio (gpio-setup "P8_3")))
     (gpio? (gpio-value-set! gpio LOW))))
  (test-assert
   (let ((gpio (gpio-setup "P9_16")))
     (gpio? (gpio-value-set! gpio HIGH))))
  (gpio-cleanup-all))
 (test-group-with-cleanup
  "setting the gpio to ouput"
  (test-equal
   "0"
   (let ((gpio (gpio-setup "P8_3")))
     (gpio-direction-set! gpio OUTPUT)
     (gpio-value-set! gpio LOW)
     (call-with-input-file
	 (string-append (gpio-sysfs-path (gpio-number-lookup "P8_3")) "/value")
       (lambda (port)
	 (read-line port)))))
  (gpio-cleanup-all))
 (test-group-with-cleanup
  "setting the gpio to input"
  (test-equal
   "1"
   (let ((gpio (gpio-setup "P8_3")))
     (gpio-direction-set! gpio OUTPUT)
     (gpio-value-set! gpio HIGH)
     (call-with-input-file
	 (string-append (gpio-sysfs-path (gpio-number-lookup "P8_3")) "/value")
       (lambda (port)
	 (read-line port)))))
  (gpio-cleanup-all)))


(test-group-with-cleanup
 "returns the correct value for a gpio"
 (test-equal
  HIGH
  (let ((gpio (gpio-setup "P9_14")))
    (gpio-direction-set! gpio OUTPUT)
    (gpio-value-set! gpio HIGH)
    (gpio-value gpio)))
 (gpio-cleanup-all)
 (test-equal
  LOW
  (let ((gpio (gpio-setup "P9_14")))
    (gpio-direction-set! gpio OUTPUT)
    (gpio-value-set! gpio LOW)
    (gpio-value gpio))))

(test-group-with-cleanup
 "returns correct boolean when comparing gpios"
 (test-assert
  (let ((gpio1 (gpio-setup "P8_3"))
	(gpio2 (gpio-setup "P8_4")))
    (not (equal? gpio1 gpio2))))
 (test-assert
  (let ((gpio (gpio-setup "P9_14")))
    (equal? gpio gpio))))

(test-end "gpio")

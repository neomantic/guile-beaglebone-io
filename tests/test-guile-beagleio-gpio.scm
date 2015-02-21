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

(test-eq 53 (gpio-number-lookup "USR0"))
(test-eq 54 (gpio-number-lookup "USR1"))
(test-eq 55 (gpio-number-lookup "USR2"))
(test-eq 56 (gpio-number-lookup "USR3"))
(test-eq 38 (gpio-number-lookup "P8_3"))
(test-eq 39 (gpio-number-lookup "P8_4"))
(test-eq 34 (gpio-number-lookup "P8_5"))
(test-eq 35 (gpio-number-lookup "P8_6"))
(test-eq 50 (gpio-number-lookup "P9_14"))
(test-eq 51 (gpio-number-lookup "P9_16"))

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
 (gpio-cleanup))

(test-group-with-cleanup
 "a gpio is setup with an INPUT direction"
 (test-equal
  INPUT
  (let ((gpio (gpio-setup "P9_16")))
    (gpio-direction gpio)))
 (gpio-cleanup))

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
  (gpio-cleanup))
 (test-group-with-cleanup
  (test-equal
   "out"
   (let ((gpio (gpio-setup "P8_3")))
     (gpio-direction-set! gpio OUTPUT)
     (call-with-input-file
	 (string-append (gpio-sysfs-path (gpio-number-lookup "P8_3")) "/direction")
       (lambda (port)
	 (read-line port)))))
  (gpio-cleanup))
 (test-group-with-cleanup
  "returns a gpio object"
  (test-assert
   (let ((gpio (gpio-setup "P8_3")))
     (gpio? (gpio-direction-set! gpio OUTPUT))))
  (test-assert
   (let ((gpio (gpio-setup "P8_3")))
     (gpio? (gpio-direction-set! gpio INPUT))))
  (gpio-cleanup)))

(test-group-with-cleanup
 "returns the correct boolean when testing if value is a gpio connection"
 (test-assert
  (not (gpio? 199)))
 (test-assert
  (gpio? (gpio-setup "P8_3")))
 (gpio-cleanup))

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
 (gpio-cleanup))

(test-group
 "cleanup exports"
 (test-assert
  (let* ((names '("P8_3" "P9_16"))
	 (pins (map gpio-number-lookup names)))
    (map (lambda (name) (gpio-setup name)) names)
    (gpio-cleanup)
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
  (gpio-cleanup))
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
  (gpio-cleanup))
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
  (gpio-cleanup)))


(test-group-with-cleanup
 "returns the correct value for a gpio"
 (test-equal
  HIGH
  (let ((gpio (gpio-setup "P9_14")))
    (gpio-direction-set! gpio OUTPUT)
    (gpio-value-set! gpio HIGH)
    (gpio-value gpio)))
 (gpio-cleanup)
 (test-equal
  LOW
  (let ((gpio (gpio-setup "P9_14")))
    (gpio-direction-set! gpio OUTPUT)
    (gpio-value-set! gpio LOW)
    (gpio-value gpio))))

(test-end "gpio")

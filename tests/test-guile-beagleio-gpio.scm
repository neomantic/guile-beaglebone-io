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

(test-group
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
  (gpio-setup "P9_16")))

(test-assert (number? INPUT))
(test-assert (number? OUTPUT))
(test-assert (not (equal? INPUT OUTPUT)))

(test-group
 "setting the gpio direction"
 (test-equal
  "in"
  (let ((gpio (gpio-setup "P8_3")))
    (gpio-direction-set! gpio INPUT)
    (call-with-input-file
	(string-append (gpio-sysfs-path (gpio-number-lookup "P8_3")) "/direction")
      (lambda (port)
	(read-line port)))))
 (test-equal
  "out"
  (let ((gpio (gpio-setup "P8_3")))
    (gpio-direction-set! gpio OUTPUT)
    (call-with-input-file
	(string-append (gpio-sysfs-path (gpio-number-lookup "P8_3")) "/direction")
      (lambda (port)
	(read-line port)))))
 (test-group
  "returns a gpio object"
  (test-assert
   (let ((gpio (gpio-setup "P8_3")))
     (gpio? (gpio-direction-set! gpio OUTPUT))))
  (test-assert
   (let ((gpio (gpio-setup "P8_3")))
     (gpio? (gpio-direction-set! gpio INPUT))))))

(test-group
 "returns the correct boolean when testing if value is a gpio connection"
 (test-assert
  (not (gpio? 199)))
 (test-assert
  (gpio? (gpio-setup "P8_3"))))

(test-group
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
    (gpio-direction gpio))))

(test-group
 "cleanup exports"
 (test-assert
  (let* ((names '("P8_3" "P9_16"))
	 (pins (map gpio-number-lookup names)))
    (map (lambda (name) (gpio-setup name)) names)
    (gpio-cleanup)
    (not (member #t (map sysfs-exists? pins))))))

(test-end "gpio")

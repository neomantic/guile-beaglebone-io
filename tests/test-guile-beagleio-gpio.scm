(add-to-load-path (string-append (dirname (dirname (current-filename)))))

(define-module (tests test-guile-beagleio)
  #:use-module (srfi srfi-64)
  #:use-module (beagleio))



(test-begin "gpio-number-lookup")

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

(test-end "gpio-number-lookup")

(test-begin "gpio-setup")
(test-error 'gpio-error (gpio-setup "non existing name"))

(define-syntax test-gpio-sysfs-export
  (syntax-rules ()
    ((_ (proc name) ...)
     (test-group
      "testing gpio-setup export to sysfs"
      (test-assert
       (let ((pin (gpio-number-lookup name)))
	 (apply proc (list name))
	 (access? (string-append "/sys/class/gpio/gpio" (number->string pin)) F_OK)
	  #t)) ...))))

(test-gpio-sysfs-export
 (gpio-setup "P8_3")
 (gpio-setup "P8_4")
 (gpio-setup "P8_5")
 (gpio-setup "P8_6")
 (gpio-setup "P9_14")
 (gpio-setup "P9_16"))

(test-end "gpio-setup")

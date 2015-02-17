(add-to-load-path (string-append (dirname (dirname (current-filename)))))

(define-module (tests test-guile-beagleio)
  #:use-module (srfi srfi-64)
  #:use-module (beagleio))



(test-begin "gpio-number-lookup")
(define-syntax test-gpio-numbers
  (syntax-rules (=>)
    ((_ (name => pin) ...)
     (test-group
      "testing lookup"
      (test-eq pin (gpio-number-lookup name))
      ...))))
(test-gpio-numbers
 ("USR0" => 53)
 ("USR1" => 54)
 ("USR2" => 55)
 ("USR3" => 56)
 ;; P8
 ;;("P8_1" => 0)
 ;;("P8_2" => 0)
 ("P8_3" => 38)
 ("P8_4" => 39)
 ("P8_5" => 34)
 ("P8_6" => 35)

 ("P9_16" => 51)
 ("P9_16" => 51))

(test-end "gpio-number-lookup")


(test-begin "gpio-setup")
(test-error 'gpio-error (gpio-setup "Hello"))
(test-assert
	 (begin
	   (gpio-setup "P9_16")
	   (access? "/sys/class/gpio/gpio51"  F_OK)))
(test-end "gpio-setup")

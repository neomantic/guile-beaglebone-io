(add-to-load-path (string-append (dirname (dirname (current-filename)))))

(define-module (tests test-guile-beagleio)
  #:use-module (srfi srfi-64)
  #:use-module (beagleio))

(test-begin "test-guile-beagleio-gpio")
(display (gpio-setup "P8_1"))
(test-end "test-guile-beagleio-gpio")

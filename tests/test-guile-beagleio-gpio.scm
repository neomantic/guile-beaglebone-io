(add-to-load-path (string-append (dirname (dirname (current-filename)))))

(define-module (tests test-guile-beagleio)
  #:use-module (srfi srfi-64)
  #:use-module (beagleio))

(test-begin "test-guile-beagleio-gpio")
(display (gpio-number-lookup "P9_16"))
(newline)

(let ((gpio (gpio-setup "P9_16")))
  (gpio-direction-set! gpio 'output))

(test-end "test-guile-beagleio-gpio")

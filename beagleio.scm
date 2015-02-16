(define-module (beagleio)
  #:use-module ((ice-9 match))
  #:export (gpio-setup
	    gpio-direction-set!
	    gpio-number-lookup))

(dynamic-call "scm_init_beagleio_gpio"
	      (dynamic-link "guile-beagleio/.libs/libguile-beagleio"))

(define (gpio-direction-set! gpio pud)
    (%gpio-direction-set!
     gpio
     (match pud
       ('input INPUT)
       ('output OUTPUT)
       (_ OUTPUT))))

(define-module (beagleio)
  #:use-module ((ice-9 match))
  #:export (gpio-setup
	    gpio-direction-set!
	    gpio-direction-get
	    gpio-number-lookup
	    INPUT
	    OUTPUT
	    HIGH
	    LOW))

(dynamic-call "scm_init_beagleio_gpio"
	      (dynamic-link "guile-beagleio/.libs/libguile-beagleio"))

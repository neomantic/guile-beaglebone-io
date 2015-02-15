(define-module (beagleio)
  #:export (gpio-setup))

(dynamic-call "scm_init_beagleio_gpio"
	      (dynamic-link "guile-beagleio/.libs/libguile-beagleio"))

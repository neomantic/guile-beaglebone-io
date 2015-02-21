#!/usr/local/bin/guile -s
!#
(add-to-load-path (string-append (dirname (dirname (current-filename)))))

(use-modules
 (srfi srfi-27)
 (ice-9 match)
 (ice-9 threads)
 (beagleio))

;; returns a random gpio level
(define (random-gpio-switch gpio-id)
  (let ((gpio (gpio-setup gpio-id)))
    (gpio-direction-set! gpio OUTPUT)
    (lambda ()
      (gpio-value-set! gpio (match
			      (random-integer 2)
			      (0 LOW)
			      (1 HIGH)
			      (_ LOW))))))


(let run ((switched '())
	  (unswitched (map random-gpio-switch '("P9_14" "P9_16" "P8_19"))))
  (if (null? unswitched)
      (run '()
	   switched)
      (let ((to-switch (car unswitched)))
	(usleep 90000)
	(to-switch)
	(run (cons to-switch switched)
	     (cdr unswitched)))))

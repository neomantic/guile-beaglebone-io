lib_LTLIBRARIES = libguile-beagleio.la
AM_CFLAGS = -Wall -O2 -std=c99 -D_POSIX_C_SOURCE=199309L @CFLAGS@  -I$(srcdir)
libguile_beagleio_la_SOURCES = c_adc.c c_adc.h \
	common.c  common.h \
	constants.c constants.h \
	c_pwm.c c_pwm.h \
	c_uart.c c_uart.h \
	event_gpio.c event_gpio.h \
	python.h py_adc.c py_pwm.c py_gpio.c py_uart.c spimodule.c \
	guile_beagleio_gpio.h guile_beagleio_gpio.c
libguile_beagleio_la_CFLAGS = $(AM_CFLAGS) @guile_CFLAGS@
libguile_beagleio_la_LIBADD = @guile_LIBS@
libguile_beagleio_la_LDFLAGS = -export-dynamic -module -avoid-version
noinst_HEADERS = common.h constants.h c_pwm.h c_uart.h event_gpio.h python.h guile_beagleio_gpio.h

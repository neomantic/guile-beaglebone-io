AM_CFLAGS = --pedantic -Wall -O2 -std=c99 -D_POSIX_C_SOURCE=199309L @CFLAGS@ @guile_CFLAGS@
AM_LDFLAGS =
LIBS = @LIBS@ @guile_LIBS@
bin_PROGRAMS = beaglebone
beaglebone_SOURCES = main.c c_adc.c c_adc.h \
		common.c  common.h \
		constants.c constants.h \
		c_pwm.c c_pwm.h \
		c_uart.c c_uart.h \
		event_gpio.c event_gpio.h \
	        phython.h py_adc.c py_pwm.c py_gpio.c py_uart.c spimodule.c

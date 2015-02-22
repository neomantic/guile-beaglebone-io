lib_LTLIBRARIES = libbeagebone-io.la
AM_CFLAGS = -Wall -O2 -std=c99 -D_POSIX_C_SOURCE=199309L @CFLAGS@  -I$(srcdir)
libbeagebone_io_la_SOURCES = common.c  common.h event_gpio.c event_gpio.h
libbeagebone_io_la_CFLAGS = $(AM_CFLAGS) -pthread
libbeagebone_io_la_LIBADD =
libbeagebone_io_la_LDFLAGS = -export-dynamic -module -avoid-version
noinst_HEADERS = common.h event_gpio.h

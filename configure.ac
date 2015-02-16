#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])
AC_INIT([beaglebone-io], [0.0.1], [calbers@neomantic.com])
AC_CONFIG_SRCDIR([guile-beagleio])
AC_CONFIG_HEADERS([config.h])
AM_INIT_AUTOMAKE

AC_CONFIG_MACRO_DIR([m4])

LT_INIT

AC_ENABLE_SHARED
AC_DISABLE_STATIC

# Checks for programs.
AC_PROG_CC

# Checks for libraries.
PKG_CHECK_MODULES([guile],[guile-2.0 >= 2.0.0])

# Checks for header files.
AC_CHECK_HEADERS([fcntl.h stddef.h stdlib.h string.h sys/ioctl.h unistd.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_TYPE_SIZE_T

# Checks for library functions.
AC_FUNC_MALLOC
AC_CHECK_FUNCS([strstr])

AC_CONFIG_FILES([Makefile
                 guile-beagleio/Makefile])

AC_OUTPUT

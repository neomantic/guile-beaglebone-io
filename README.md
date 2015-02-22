# guile beaglebone io

A guile scheme library for the beaglebone.  It is based off the c
source code found at [https://github.com/adafruit/adafruit-beaglebone-io-python](https://github.com/adafruit/adafruit-beaglebone-io-python)

Currently, the master branch is not functional.  All development is on the
develop branch.

# Build

1. autoreconf -vfi
2. ./configure
3. make

# Install

The library currently doesn't install

# Local tests

On beaglebone black debian, the gpio sysfs is only accessible to root. So
all tests must be run as sudo

```
sudo guile tests/*
```

# Example

See `example/example.scm`

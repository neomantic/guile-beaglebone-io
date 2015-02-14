#ifndef GUILE_BEAGLEIO_GPIO_H
#define GUILE_BEAGLEIO_GPIO_H

#include <libguile.h>

struct gpio {
  unsigned int pin_number;
  SCM channel;
  SCM update_func;
};

static scm_t_bits gpio_tag;

void scm_init_beagleio_gpio(void *unused);
void scm_init_beagleio_gpio_module();

#endif

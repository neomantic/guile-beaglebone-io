#ifndef SCM_GPIO_H
#define SCM_GPIO_H

#include <libguile.h>

struct gpio {
  unsigned int pin_number;
  SCM channel;
  SCM update_func;
};

static scm_t_bits gpio_tag;

SCM setup_channel(SCM s_channel);
void init_gpio_type(void);
void init_beagleio_gpio(void *unused);
void scm_init_beagleio_gpio_module();

#endif

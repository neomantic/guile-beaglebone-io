#include <libguile.h>
#include "common.h"
#include "event_gpio.h"
#include "scm_gpio.h"

SCM
setup_channel(SCM s_channel) {
  unsigned int gpio_number;
  char *channel = scm_to_locale_string(s_channel);

  if (get_gpio_number(channel, &gpio_number))
    return NULL;

  gpio_export(gpio_number);

  SCM smob;
  struct gpio *gpio;
  scm_gc_malloc(sizeof(struct gpio), "gpio");
  gpio->pin_number = gpio_number;
  gpio->channel = s_channel;
  gpio->update_func = SCM_BOOL_F;
  smob = scm_new_smob(gpio_tag, (scm_t_bits) gpio);
  return smob;
}

static int
print_gpio(SCM gpio_smob, SCM port, scm_print_state *pstate) {
  struct gpio *gpio = (struct gpio*) SCM_SMOB_DATA(gpio_smob);
  scm_puts("#<gpio channel=", port);
  scm_display(gpio->channel, port);
  scm_puts(" pin=", port);
  scm_display(scm_from_unsigned_integer(gpio->pin_number), port);
  scm_puts(" >", port);
  return 1;
}

static size_t
free_gpio(SCM gpio_smob) {
  struct gpio *gpio = (struct gpio *) SCM_SMOB_DATA(gpio_smob);
  scm_gc_free(gpio, sizeof(struct gpio), "gpio");
  return 0;
}

static SCM
mark_gpio(SCM gpio_smob) {
  struct gpio *gpio = (struct gpio *) SCM_SMOB_DATA (gpio_smob);
  scm_gc_mark(gpio->channel);
  return (SCM) gpio->update_func;
}

static SCM
equal_gpio(SCM gpio_smob, SCM other_gpio_smob ){
  struct gpio *gpio = (struct gpio *) SCM_SMOB_DATA (gpio_smob);
  struct gpio *other = (struct gpio *) SCM_SMOB_DATA (other_gpio_smob);
  //return SCM_BOOL_F;
  if ( gpio->pin_number == other->pin_number)
    {
      return SCM_BOOL_T;
    }
  else
    {
      return SCM_BOOL_F;
    }
}

void
sinit_gpio_type(void *unused) {
  gpio_tag = scm_make_smob_type("gpio", sizeof(struct gpio));
  scm_set_smob_print(gpio_tag, print_gpio);
  scm_set_smob_free(gpio_tag, free_gpio);
  scm_set_smob_mark(gpio_tag, mark_gpio);
  scm_set_smob_equalp(gpio_tag, equal_gpio);
}

void
init_beagleio_gpio(void *unused) {
  scm_c_define_gsubr("setup", 1, 0, 0, setup_channel);
  scm_c_export("setup", NULL);
}

void
scm_init_beagleio_gpio_module() {
  scm_c_define_module("beagleio gpio", init_beagleio_gpio, NULL);
}

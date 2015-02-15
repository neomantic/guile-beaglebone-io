#include <stdlib.h>
#include <libguile.h>
#include "common.h"
#include "event_gpio.h"
#include "guile_beagleio_gpio.h"

static scm_t_bits gpio_tag;

struct gpio {
  unsigned int pin_number;
  SCM channel;
  SCM update_func;
};

SCM
setup_channel(SCM s_channel) {
  SCM smob;
  struct gpio *gpio;
  unsigned int gpio_number;
  char *channel = scm_to_locale_string(s_channel);
  int exported;

  if (get_gpio_number(channel, &gpio_number)) {
    scm_throw(scm_from_utf8_symbol("gpio-error"), scm_from_utf8_string("unable to find pin number"));
  }

  exported = gpio_export(gpio_number);
  if (exported != 0 ) {
    scm_throw(scm_from_utf8_symbol("gpio-error"), scm_from_utf8_string("unable to export"));
  }

  gpio = (struct gpio *) scm_gc_malloc(sizeof(struct gpio), "gpio");
  gpio->pin_number = gpio_number;
  gpio->channel = SCM_BOOL_F;
  gpio->update_func = SCM_BOOL_F;

  smob = scm_new_smob(gpio_tag, (scm_t_bits) gpio);
  gpio->channel = s_channel;
  return smob;
}

static int
print_gpio(SCM gpio_smob, SCM port, scm_print_state *pstate) {
  struct gpio *gpio;
  scm_assert_smob_type(gpio_tag, gpio_smob);
  gpio = (struct gpio*) SCM_SMOB_DATA(gpio_smob);
  scm_puts("#<gpio channel=", port);
  scm_display(gpio->channel, port);
  scm_puts(" pin=", port);
  scm_display(scm_from_unsigned_integer(gpio->pin_number), port);
  scm_puts(">", port);
  return 1;
}

static size_t
free_gpio(SCM gpio_smob) {
  scm_assert_smob_type(gpio_tag, gpio_smob);
  struct gpio *gpio = (struct gpio *) SCM_SMOB_DATA(gpio_smob);
  scm_gc_free(gpio, sizeof(struct gpio), "gpio");
  return 0;
}

static SCM
mark_gpio(SCM gpio_smob) {
  scm_assert_smob_type(gpio_tag, gpio_smob);
  struct gpio *gpio = (struct gpio *) SCM_SMOB_DATA (gpio_smob);
  scm_gc_mark(gpio->channel);
  return (SCM) gpio->update_func;
}

static SCM
equal_gpio(SCM gpio_smob, SCM other_gpio_smob ){
  struct gpio *gpio = (struct gpio *) SCM_SMOB_DATA (gpio_smob);
  struct gpio *other = (struct gpio *) SCM_SMOB_DATA (other_gpio_smob);
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
init_gpio_type(void) {
  gpio_tag = scm_make_smob_type("gpio", sizeof(struct gpio));
  scm_set_smob_print(gpio_tag, print_gpio);
  scm_set_smob_free(gpio_tag, free_gpio);
  scm_set_smob_mark(gpio_tag, mark_gpio);
  //scm_set_smob_equalp(gpio_tag, equal_gpio);
}

void
scm_init_beagleio_gpio(void) {
  init_gpio_type();
  scm_c_define_gsubr("gpio-setup", 1, 0, 0, setup_channel);
}
#include "model_c_write.h"

static void (*c2m3write)(void *, unsigned long value)=(void *)0;

void
write_field(void *field, unsigned long value)
{
  c2m3write(field, value);
}

void
set_model_c2m3callback(void (*f)(void *, unsigned long value))
{
  c2m3write = f;
}

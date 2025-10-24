#include <stdio.h>
#include <gmp.h>

extern char *mpz_format_decimal(const mpz_t z);

int main()
{
  mpz_t x, y, result;

  // Initialize mpz_t variables
  mpz_init(x);
  mpz_init(y);
  mpz_init(result);

  // Set values using strings (base 10)
  mpz_set_str(x, "123", 10);
  mpz_set_str(y, "456", 10);

  // Perform multiplication
  mpz_pow_ui(result, x, 10);

  // Print the result
  printf("x = ");
  gmp_printf("%Zd\n", x); // gmp_printf for printing mpz_t
  printf("y = ");
  gmp_printf("%Zd\n", y);
  printf("x * y = ");
  gmp_printf("%Zd\n", result);

  {
    char *s = mpz_format_decimal(result);

    printf("s = %s\n", s);
  }
      

  // Clear memory (important to avoid memory leaks)
  mpz_clear(x);
  mpz_clear(y);
  mpz_clear(result);
  return 0;
}

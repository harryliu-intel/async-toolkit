INTERFACE MpzOps;
IMPORT Mpz;
IMPORT Word;

TYPE T       = Mpz.T;

PROCEDURE mpz_abs (f0 : T; f1 : T);

PROCEDURE mpz_add (f0 : T; f1 : T; f2 : T);

PROCEDURE mpz_add_ui (f0 : T; f1 : T; f2 : Word.T);

PROCEDURE mpz_addmul (f0 : T; f1 : T; f2 : T);

PROCEDURE mpz_addmul_ui (f0 : T; f1 : T; f2 : Word.T);

PROCEDURE mpz_and (f0 : T; f1 : T; f2 : T);

PROCEDURE mpz_bin_ui (f0 : T; f1 : T; f2 : Word.T);

PROCEDURE mpz_bin_uiui (f0 : T; f1 : Word.T; f2 : Word.T);

PROCEDURE mpz_cdiv_q (f0 : T; f1 : T; f2 : T);

PROCEDURE mpz_cdiv_q_2exp (f0 : T; f1 : T; f2 : Word.T);

PROCEDURE mpz_cdiv_q_ui (f0 : T; f1 : T; f2 : Word.T) : Word.T;

PROCEDURE mpz_cdiv_qr (f0 : T; f1 : T; f2 : T; f3 : T);

PROCEDURE mpz_cdiv_qr_ui (f0 : T; f1 : T; f2 : T; f3 : Word.T) : Word.T;

PROCEDURE mpz_cdiv_r (f0 : T; f1 : T; f2 : T);

PROCEDURE mpz_cdiv_r_2exp (f0 : T; f1 : T; f2 : Word.T);

PROCEDURE mpz_cdiv_r_ui (f0 : T; f1 : T; f2 : Word.T) : Word.T;

PROCEDURE mpz_cdiv_ui (f0 : T; f1 : Word.T) : Word.T;

PROCEDURE mpz_clear (f0 : T);

PROCEDURE mpz_clrbit (f0 : T; f1 : Word.T);

PROCEDURE mpz_cmp (f0 : T; f1 : T) : INTEGER;

PROCEDURE mpz_cmp_d (f0 : T; f1 : LONGREAL) : INTEGER;

PROCEDURE mpz_cmpabs (f0 : T; f1 : T) : INTEGER;

PROCEDURE mpz_cmpabs_d (f0 : T; f1 : LONGREAL) : INTEGER;

PROCEDURE mpz_cmpabs_ui (f0 : T; f1 : Word.T) : INTEGER;

PROCEDURE mpz_com (f0 : T; f1 : T);

PROCEDURE mpz_combit (f0 : T; f1 : Word.T);

PROCEDURE mpz_congruent_p (f0 : T; f1 : T; f2 : T) : INTEGER;

PROCEDURE mpz_congruent_2exp_p (f0 : T; f1 : T; f2 : Word.T) : INTEGER;

PROCEDURE mpz_congruent_ui_p (f0 : T; f1 : Word.T; f2 : Word.T) : INTEGER;

PROCEDURE mpz_divexact (f0 : T; f1 : T; f2 : T);

PROCEDURE mpz_divexact_ui (f0 : T; f1 : T; f2 : Word.T);

PROCEDURE mpz_divisible_p (f0 : T; f1 : T) : INTEGER;

PROCEDURE mpz_divisible_ui_p (f0 : T; f1 : Word.T) : INTEGER;

PROCEDURE mpz_divisible_2exp_p (f0 : T; f1 : Word.T) : INTEGER;

PROCEDURE mpz_dump (f0 : T);

PROCEDURE mpz_fac_ui (f0 : T; f1 : Word.T);

PROCEDURE mpz_fdiv_q (f0 : T; f1 : T; f2 : T);

PROCEDURE mpz_fdiv_q_2exp (f0 : T; f1 : T; f2 : Word.T);

PROCEDURE mpz_fdiv_q_ui (f0 : T; f1 : T; f2 : Word.T) : Word.T;

PROCEDURE mpz_fdiv_qr (f0 : T; f1 : T; f2 : T; f3 : T);

PROCEDURE mpz_fdiv_qr_ui (f0 : T; f1 : T; f2 : T; f3 : Word.T) : Word.T;

PROCEDURE mpz_fdiv_r (f0 : T; f1 : T; f2 : T);

PROCEDURE mpz_fdiv_r_2exp (f0 : T; f1 : T; f2 : Word.T);

PROCEDURE mpz_fdiv_r_ui (f0 : T; f1 : T; f2 : Word.T) : Word.T;

PROCEDURE mpz_fdiv_ui (f0 : T; f1 : Word.T) : Word.T;

PROCEDURE mpz_fib_ui (f0 : T; f1 : Word.T);

PROCEDURE mpz_fib2_ui (f0 : T; f1 : T; f2 : Word.T);

PROCEDURE mpz_fits_sint_p (f0 : T) : INTEGER;

PROCEDURE mpz_fits_slong_p (f0 : T) : INTEGER;

PROCEDURE mpz_fits_sshort_p (f0 : T) : INTEGER;

PROCEDURE mpz_fits_uint_p (f0 : T) : INTEGER;

PROCEDURE mpz_fits_ulong_p (f0 : T) : INTEGER;

PROCEDURE mpz_fits_ushort_p (f0 : T) : INTEGER;

PROCEDURE mpz_gcd (f0 : T; f1 : T; f2 : T);

PROCEDURE mpz_gcd_ui (f0 : T; f1 : T; f2 : Word.T) : Word.T;

PROCEDURE mpz_gcdext (f0 : T; f1 : T; f2 : T; f3 : T; f4 : T);

PROCEDURE mpz_get_d (f0 : T) : LONGREAL;

PROCEDURE mpz_get_ui (f0 : T) : Word.T;

PROCEDURE mpz_hamdist (f0 : T; f1 : T) : Word.T;

PROCEDURE mpz_init (f0 : T);

PROCEDURE mpz_init2 (f0 : T; f1 : Word.T);

PROCEDURE mpz_init_set (f0 : T; f1 : T);

PROCEDURE mpz_init_set_d (f0 : T; f1 : LONGREAL);

PROCEDURE mpz_init_set_si (f0 : T; f1 : INTEGER);

PROCEDURE mpz_init_set_str (f0 : T; f1 : TEXT; f2 : INTEGER) : INTEGER;

PROCEDURE mpz_init_set_ui (f0 : T; f1 : Word.T);

PROCEDURE mpz_invert (f0 : T; f1 : T; f2 : T) : INTEGER;

PROCEDURE mpz_ior (f0 : T; f1 : T; f2 : T);

PROCEDURE mpz_jacobi (f0 : T; f1 : T) : INTEGER;

PROCEDURE mpz_kronecker_ui (f0 : T; f1 : Word.T) : INTEGER;

PROCEDURE mpz_ui_kronecker (f0 : Word.T; f1 : T) : INTEGER;

PROCEDURE mpz_lcm (f0 : T; f1 : T; f2 : T);

PROCEDURE mpz_lcm_ui (f0 : T; f1 : T; f2 : Word.T);

PROCEDURE mpz_lucnum_ui (f0 : T; f1 : Word.T);

PROCEDURE mpz_lucnum2_ui (f0 : T; f1 : T; f2 : Word.T);

PROCEDURE mpz_millerrabin (f0 : T; f1 : INTEGER) : INTEGER;

PROCEDURE mpz_mod (f0 : T; f1 : T; f2 : T);

PROCEDURE mpz_mul (f0 : T; f1 : T; f2 : T);

PROCEDURE mpz_mul_2exp (f0 : T; f1 : T; f2 : Word.T);

PROCEDURE mpz_mul_ui (f0 : T; f1 : T; f2 : Word.T);

PROCEDURE mpz_neg (f0 : T; f1 : T);

PROCEDURE mpz_nextprime (f0 : T; f1 : T);

PROCEDURE mpz_perfect_power_p (f0 : T) : INTEGER;

PROCEDURE mpz_perfect_square_p (f0 : T) : INTEGER;

PROCEDURE mpz_popcount (f0 : T) : Word.T;

PROCEDURE mpz_pow_ui (f0 : T; f1 : T; f2 : Word.T);

PROCEDURE mpz_powm (f0 : T; f1 : T; f2 : T; f3 : T);

PROCEDURE mpz_powm_ui (f0 : T; f1 : T; f2 : Word.T; f3 : T);

PROCEDURE mpz_probab_prime_p (f0 : T; f1 : INTEGER) : INTEGER;

PROCEDURE mpz_realloc2 (f0 : T; f1 : Word.T);

PROCEDURE mpz_remove (f0 : T; f1 : T; f2 : T) : Word.T;

PROCEDURE mpz_root (f0 : T; f1 : T; f2 : Word.T) : INTEGER;

PROCEDURE mpz_rootrem (f0 : T; f1 : T; f2 : T; f3 : Word.T);

PROCEDURE mpz_scan0 (f0 : T; f1 : Word.T) : Word.T;

PROCEDURE mpz_scan1 (f0 : T; f1 : Word.T) : Word.T;

PROCEDURE mpz_set (f0 : T; f1 : T);

PROCEDURE mpz_set_d (f0 : T; f1 : LONGREAL);

PROCEDURE mpz_set_si (f0 : T; f1 : INTEGER);

PROCEDURE mpz_set_str (f0 : T; f1 : TEXT; f2 : INTEGER) : INTEGER;

PROCEDURE mpz_set_ui (f0 : T; f1 : Word.T);

PROCEDURE mpz_setbit (f0 : T; f1 : Word.T);

PROCEDURE mpz_size (f0 : T) : CARDINAL;

PROCEDURE mpz_sizeinbase (f0 : T; f1 : INTEGER) : CARDINAL;

PROCEDURE mpz_sqrt (f0 : T; f1 : T);

PROCEDURE mpz_sqrtrem (f0 : T; f1 : T; f2 : T);

PROCEDURE mpz_sub (f0 : T; f1 : T; f2 : T);

PROCEDURE mpz_sub_ui (f0 : T; f1 : T; f2 : Word.T);

PROCEDURE mpz_ui_sub (f0 : T; f1 : Word.T; f2 : T);

PROCEDURE mpz_submul (f0 : T; f1 : T; f2 : T);

PROCEDURE mpz_submul_ui (f0 : T; f1 : T; f2 : Word.T);

PROCEDURE mpz_swap (f0 : T; f1 : T);

PROCEDURE mpz_tdiv_ui (f0 : T; f1 : Word.T) : Word.T;

PROCEDURE mpz_tdiv_q (f0 : T; f1 : T; f2 : T);

PROCEDURE mpz_tdiv_q_2exp (f0 : T; f1 : T; f2 : Word.T);

PROCEDURE mpz_tdiv_q_ui (f0 : T; f1 : T; f2 : Word.T) : Word.T;

PROCEDURE mpz_tdiv_qr (f0 : T; f1 : T; f2 : T; f3 : T);

PROCEDURE mpz_tdiv_qr_ui (f0 : T; f1 : T; f2 : T; f3 : Word.T) : Word.T;

PROCEDURE mpz_tdiv_r (f0 : T; f1 : T; f2 : T);

PROCEDURE mpz_tdiv_r_2exp (f0 : T; f1 : T; f2 : Word.T);

PROCEDURE mpz_tdiv_r_ui (f0 : T; f1 : T; f2 : Word.T) : Word.T;

PROCEDURE mpz_tstbit (f0 : T; f1 : Word.T) : INTEGER;

PROCEDURE mpz_ui_pow_ui (f0 : T; f1 : Word.T; f2 : Word.T);

PROCEDURE mpz_xor (f0 : T; f1 : T; f2 : T);

PROCEDURE mpf_set_default_prec (f0 : Word.T);

END MpzOps.

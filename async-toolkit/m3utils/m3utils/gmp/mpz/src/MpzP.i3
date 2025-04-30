INTERFACE MpzP;
IMPORT Word;
IMPORT Ctypes;

TYPE T       = MpzPtrT;
     MpzPtrT = ADDRESS;

(***** hand-coded functions *****)
<*EXTERNAL mpz_format_octal*>
PROCEDURE mpz_format_octal(f0 : MpzPtrT) : Ctypes.const_char_star;

<*EXTERNAL mpz_format_decimal*>
PROCEDURE mpz_format_decimal(f0 : MpzPtrT) : Ctypes.const_char_star;

<*EXTERNAL mpz_format_hexadecimal*>
PROCEDURE mpz_format_hexadecimal(f0 : MpzPtrT) : Ctypes.const_char_star;

<*EXTERNAL mpz_free_formatted*>
PROCEDURE mpz_free_formatted(f0 : Ctypes.char_star);


(***** auto-generated functions *****)

<*EXTERNAL "__gmpz_abs" *>
PROCEDURE c_mpz_abs (f0 : MpzPtrT; f1 : MpzPtrT);

<*EXTERNAL "__gmpz_add" *>
PROCEDURE c_mpz_add (f0 : MpzPtrT; f1 : MpzPtrT; f2 : MpzPtrT);

<*EXTERNAL "__gmpz_add_ui" *>
PROCEDURE c_mpz_add_ui (f0 : MpzPtrT; f1 : MpzPtrT; f2 : Word.T);

<*EXTERNAL "__gmpz_addmul" *>
PROCEDURE c_mpz_addmul (f0 : MpzPtrT; f1 : MpzPtrT; f2 : MpzPtrT);

<*EXTERNAL "__gmpz_addmul_ui" *>
PROCEDURE c_mpz_addmul_ui (f0 : MpzPtrT; f1 : MpzPtrT; f2 : Word.T);

<*EXTERNAL "__gmpz_and" *>
PROCEDURE c_mpz_and (f0 : MpzPtrT; f1 : MpzPtrT; f2 : MpzPtrT);

<*EXTERNAL "__gmpz_bin_ui" *>
PROCEDURE c_mpz_bin_ui (f0 : MpzPtrT; f1 : MpzPtrT; f2 : Word.T);

<*EXTERNAL "__gmpz_bin_uiui" *>
PROCEDURE c_mpz_bin_uiui (f0 : MpzPtrT; f1 : Word.T; f2 : Word.T);

<*EXTERNAL "__gmpz_cdiv_q" *>
PROCEDURE c_mpz_cdiv_q (f0 : MpzPtrT; f1 : MpzPtrT; f2 : MpzPtrT);

<*EXTERNAL "__gmpz_cdiv_q_2exp" *>
PROCEDURE c_mpz_cdiv_q_2exp (f0 : MpzPtrT; f1 : MpzPtrT; f2 : Word.T);

<*EXTERNAL "__gmpz_cdiv_q_ui" *>
PROCEDURE c_mpz_cdiv_q_ui (f0 : MpzPtrT; f1 : MpzPtrT; f2 : Word.T) : Word.T;

<*EXTERNAL "__gmpz_cdiv_qr" *>
PROCEDURE c_mpz_cdiv_qr (f0 : MpzPtrT; f1 : MpzPtrT; f2 : MpzPtrT; f3 : MpzPtrT);

<*EXTERNAL "__gmpz_cdiv_qr_ui" *>
PROCEDURE c_mpz_cdiv_qr_ui (f0 : MpzPtrT; f1 : MpzPtrT; f2 : MpzPtrT; f3 : Word.T) : Word.T;

<*EXTERNAL "__gmpz_cdiv_r" *>
PROCEDURE c_mpz_cdiv_r (f0 : MpzPtrT; f1 : MpzPtrT; f2 : MpzPtrT);

<*EXTERNAL "__gmpz_cdiv_r_2exp" *>
PROCEDURE c_mpz_cdiv_r_2exp (f0 : MpzPtrT; f1 : MpzPtrT; f2 : Word.T);

<*EXTERNAL "__gmpz_cdiv_r_ui" *>
PROCEDURE c_mpz_cdiv_r_ui (f0 : MpzPtrT; f1 : MpzPtrT; f2 : Word.T) : Word.T;

<*EXTERNAL "__gmpz_cdiv_ui" *>
PROCEDURE c_mpz_cdiv_ui (f0 : MpzPtrT; f1 : Word.T) : Word.T;

<*EXTERNAL "__gmpz_clear" *>
PROCEDURE c_mpz_clear (f0 : MpzPtrT);

<*EXTERNAL "__gmpz_clrbit" *>
PROCEDURE c_mpz_clrbit (f0 : MpzPtrT; f1 : Word.T);

<*EXTERNAL "__gmpz_cmp" *>
PROCEDURE c_mpz_cmp (f0 : MpzPtrT; f1 : MpzPtrT) : INTEGER;

<*EXTERNAL "__gmpz_cmp_d" *>
PROCEDURE c_mpz_cmp_d (f0 : MpzPtrT; f1 : LONGREAL) : INTEGER;

<*EXTERNAL "__gmpz_cmpabs" *>
PROCEDURE c_mpz_cmpabs (f0 : MpzPtrT; f1 : MpzPtrT) : INTEGER;

<*EXTERNAL "__gmpz_cmpabs_d" *>
PROCEDURE c_mpz_cmpabs_d (f0 : MpzPtrT; f1 : LONGREAL) : INTEGER;

<*EXTERNAL "__gmpz_cmpabs_ui" *>
PROCEDURE c_mpz_cmpabs_ui (f0 : MpzPtrT; f1 : Word.T) : INTEGER;

<*EXTERNAL "__gmpz_com" *>
PROCEDURE c_mpz_com (f0 : MpzPtrT; f1 : MpzPtrT);

<*EXTERNAL "__gmpz_combit" *>
PROCEDURE c_mpz_combit (f0 : MpzPtrT; f1 : Word.T);

<*EXTERNAL "__gmpz_congruent_p" *>
PROCEDURE c_mpz_congruent_p (f0 : MpzPtrT; f1 : MpzPtrT; f2 : MpzPtrT) : INTEGER;

<*EXTERNAL "__gmpz_congruent_2exp_p" *>
PROCEDURE c_mpz_congruent_2exp_p (f0 : MpzPtrT; f1 : MpzPtrT; f2 : Word.T) : INTEGER;

<*EXTERNAL "__gmpz_congruent_ui_p" *>
PROCEDURE c_mpz_congruent_ui_p (f0 : MpzPtrT; f1 : Word.T; f2 : Word.T) : INTEGER;

<*EXTERNAL "__gmpz_divexact" *>
PROCEDURE c_mpz_divexact (f0 : MpzPtrT; f1 : MpzPtrT; f2 : MpzPtrT);

<*EXTERNAL "__gmpz_divexact_ui" *>
PROCEDURE c_mpz_divexact_ui (f0 : MpzPtrT; f1 : MpzPtrT; f2 : Word.T);

<*EXTERNAL "__gmpz_divisible_p" *>
PROCEDURE c_mpz_divisible_p (f0 : MpzPtrT; f1 : MpzPtrT) : INTEGER;

<*EXTERNAL "__gmpz_divisible_ui_p" *>
PROCEDURE c_mpz_divisible_ui_p (f0 : MpzPtrT; f1 : Word.T) : INTEGER;

<*EXTERNAL "__gmpz_divisible_2exp_p" *>
PROCEDURE c_mpz_divisible_2exp_p (f0 : MpzPtrT; f1 : Word.T) : INTEGER;

<*EXTERNAL "__gmpz_dump" *>
PROCEDURE c_mpz_dump (f0 : MpzPtrT);

<*EXTERNAL "__gmpz_fac_ui" *>
PROCEDURE c_mpz_fac_ui (f0 : MpzPtrT; f1 : Word.T);

<*EXTERNAL "__gmpz_fdiv_q" *>
PROCEDURE c_mpz_fdiv_q (f0 : MpzPtrT; f1 : MpzPtrT; f2 : MpzPtrT);

<*EXTERNAL "__gmpz_fdiv_q_2exp" *>
PROCEDURE c_mpz_fdiv_q_2exp (f0 : MpzPtrT; f1 : MpzPtrT; f2 : Word.T);

<*EXTERNAL "__gmpz_fdiv_q_ui" *>
PROCEDURE c_mpz_fdiv_q_ui (f0 : MpzPtrT; f1 : MpzPtrT; f2 : Word.T) : Word.T;

<*EXTERNAL "__gmpz_fdiv_qr" *>
PROCEDURE c_mpz_fdiv_qr (f0 : MpzPtrT; f1 : MpzPtrT; f2 : MpzPtrT; f3 : MpzPtrT);

<*EXTERNAL "__gmpz_fdiv_qr_ui" *>
PROCEDURE c_mpz_fdiv_qr_ui (f0 : MpzPtrT; f1 : MpzPtrT; f2 : MpzPtrT; f3 : Word.T) : Word.T;

<*EXTERNAL "__gmpz_fdiv_r" *>
PROCEDURE c_mpz_fdiv_r (f0 : MpzPtrT; f1 : MpzPtrT; f2 : MpzPtrT);

<*EXTERNAL "__gmpz_fdiv_r_2exp" *>
PROCEDURE c_mpz_fdiv_r_2exp (f0 : MpzPtrT; f1 : MpzPtrT; f2 : Word.T);

<*EXTERNAL "__gmpz_fdiv_r_ui" *>
PROCEDURE c_mpz_fdiv_r_ui (f0 : MpzPtrT; f1 : MpzPtrT; f2 : Word.T) : Word.T;

<*EXTERNAL "__gmpz_fdiv_ui" *>
PROCEDURE c_mpz_fdiv_ui (f0 : MpzPtrT; f1 : Word.T) : Word.T;

<*EXTERNAL "__gmpz_fib_ui" *>
PROCEDURE c_mpz_fib_ui (f0 : MpzPtrT; f1 : Word.T);

<*EXTERNAL "__gmpz_fib2_ui" *>
PROCEDURE c_mpz_fib2_ui (f0 : MpzPtrT; f1 : MpzPtrT; f2 : Word.T);

<*EXTERNAL "__gmpz_fits_sint_p" *>
PROCEDURE c_mpz_fits_sint_p (f0 : MpzPtrT) : INTEGER;

<*EXTERNAL "__gmpz_fits_slong_p" *>
PROCEDURE c_mpz_fits_slong_p (f0 : MpzPtrT) : INTEGER;

<*EXTERNAL "__gmpz_fits_sshort_p" *>
PROCEDURE c_mpz_fits_sshort_p (f0 : MpzPtrT) : INTEGER;

<*EXTERNAL "__gmpz_fits_uint_p" *>
PROCEDURE c_mpz_fits_uint_p (f0 : MpzPtrT) : INTEGER;

<*EXTERNAL "__gmpz_fits_ulong_p" *>
PROCEDURE c_mpz_fits_ulong_p (f0 : MpzPtrT) : INTEGER;

<*EXTERNAL "__gmpz_fits_ushort_p" *>
PROCEDURE c_mpz_fits_ushort_p (f0 : MpzPtrT) : INTEGER;

<*EXTERNAL "__gmpz_gcd" *>
PROCEDURE c_mpz_gcd (f0 : MpzPtrT; f1 : MpzPtrT; f2 : MpzPtrT);

<*EXTERNAL "__gmpz_gcd_ui" *>
PROCEDURE c_mpz_gcd_ui (f0 : MpzPtrT; f1 : MpzPtrT; f2 : Word.T) : Word.T;

<*EXTERNAL "__gmpz_gcdext" *>
PROCEDURE c_mpz_gcdext (f0 : MpzPtrT; f1 : MpzPtrT; f2 : MpzPtrT; f3 : MpzPtrT; f4 : MpzPtrT);

<*EXTERNAL "__gmpz_get_d" *>
PROCEDURE c_mpz_get_d (f0 : MpzPtrT) : LONGREAL;

<*EXTERNAL "__gmpz_get_ui" *>
PROCEDURE c_mpz_get_ui (f0 : MpzPtrT) : Word.T;

<*EXTERNAL "__gmpz_hamdist" *>
PROCEDURE c_mpz_hamdist (f0 : MpzPtrT; f1 : MpzPtrT) : Word.T;

<*EXTERNAL "__gmpz_init" *>
PROCEDURE c_mpz_init (f0 : MpzPtrT);

<*EXTERNAL "__gmpz_init2" *>
PROCEDURE c_mpz_init2 (f0 : MpzPtrT; f1 : Word.T);

<*EXTERNAL "__gmpz_init_set" *>
PROCEDURE c_mpz_init_set (f0 : MpzPtrT; f1 : MpzPtrT);

<*EXTERNAL "__gmpz_init_set_d" *>
PROCEDURE c_mpz_init_set_d (f0 : MpzPtrT; f1 : LONGREAL);

<*EXTERNAL "__gmpz_init_set_si" *>
PROCEDURE c_mpz_init_set_si (f0 : MpzPtrT; f1 : INTEGER);

<*EXTERNAL "__gmpz_init_set_str" *>
PROCEDURE c_mpz_init_set_str (f0 : MpzPtrT; f1 : Ctypes.const_char_star; f2 : INTEGER) : INTEGER;

<*EXTERNAL "__gmpz_init_set_ui" *>
PROCEDURE c_mpz_init_set_ui (f0 : MpzPtrT; f1 : Word.T);

<*EXTERNAL "__gmpz_invert" *>
PROCEDURE c_mpz_invert (f0 : MpzPtrT; f1 : MpzPtrT; f2 : MpzPtrT) : INTEGER;

<*EXTERNAL "__gmpz_ior" *>
PROCEDURE c_mpz_ior (f0 : MpzPtrT; f1 : MpzPtrT; f2 : MpzPtrT);

<*EXTERNAL "__gmpz_jacobi" *>
PROCEDURE c_mpz_jacobi (f0 : MpzPtrT; f1 : MpzPtrT) : INTEGER;

<*EXTERNAL "__gmpz_kronecker_ui" *>
PROCEDURE c_mpz_kronecker_ui (f0 : MpzPtrT; f1 : Word.T) : INTEGER;

<*EXTERNAL "__gmpz_ui_kronecker" *>
PROCEDURE c_mpz_ui_kronecker (f0 : Word.T; f1 : MpzPtrT) : INTEGER;

<*EXTERNAL "__gmpz_lcm" *>
PROCEDURE c_mpz_lcm (f0 : MpzPtrT; f1 : MpzPtrT; f2 : MpzPtrT);

<*EXTERNAL "__gmpz_lcm_ui" *>
PROCEDURE c_mpz_lcm_ui (f0 : MpzPtrT; f1 : MpzPtrT; f2 : Word.T);

<*EXTERNAL "__gmpz_lucnum_ui" *>
PROCEDURE c_mpz_lucnum_ui (f0 : MpzPtrT; f1 : Word.T);

<*EXTERNAL "__gmpz_lucnum2_ui" *>
PROCEDURE c_mpz_lucnum2_ui (f0 : MpzPtrT; f1 : MpzPtrT; f2 : Word.T);

<*EXTERNAL "__gmpz_millerrabin" *>
PROCEDURE c_mpz_millerrabin (f0 : MpzPtrT; f1 : INTEGER) : INTEGER;

<*EXTERNAL "__gmpz_mod" *>
PROCEDURE c_mpz_mod (f0 : MpzPtrT; f1 : MpzPtrT; f2 : MpzPtrT);

<*EXTERNAL "__gmpz_mul" *>
PROCEDURE c_mpz_mul (f0 : MpzPtrT; f1 : MpzPtrT; f2 : MpzPtrT);

<*EXTERNAL "__gmpz_mul_2exp" *>
PROCEDURE c_mpz_mul_2exp (f0 : MpzPtrT; f1 : MpzPtrT; f2 : Word.T);

<*EXTERNAL "__gmpz_mul_ui" *>
PROCEDURE c_mpz_mul_ui (f0 : MpzPtrT; f1 : MpzPtrT; f2 : Word.T);

<*EXTERNAL "__gmpz_neg" *>
PROCEDURE c_mpz_neg (f0 : MpzPtrT; f1 : MpzPtrT);

<*EXTERNAL "__gmpz_nextprime" *>
PROCEDURE c_mpz_nextprime (f0 : MpzPtrT; f1 : MpzPtrT);

<*EXTERNAL "__gmpz_perfect_power_p" *>
PROCEDURE c_mpz_perfect_power_p (f0 : MpzPtrT) : INTEGER;

<*EXTERNAL "__gmpz_perfect_square_p" *>
PROCEDURE c_mpz_perfect_square_p (f0 : MpzPtrT) : INTEGER;

<*EXTERNAL "__gmpz_popcount" *>
PROCEDURE c_mpz_popcount (f0 : MpzPtrT) : Word.T;

<*EXTERNAL "__gmpz_pow_ui" *>
PROCEDURE c_mpz_pow_ui (f0 : MpzPtrT; f1 : MpzPtrT; f2 : Word.T);

<*EXTERNAL "__gmpz_powm" *>
PROCEDURE c_mpz_powm (f0 : MpzPtrT; f1 : MpzPtrT; f2 : MpzPtrT; f3 : MpzPtrT);

<*EXTERNAL "__gmpz_powm_ui" *>
PROCEDURE c_mpz_powm_ui (f0 : MpzPtrT; f1 : MpzPtrT; f2 : Word.T; f3 : MpzPtrT);

<*EXTERNAL "__gmpz_probab_prime_p" *>
PROCEDURE c_mpz_probab_prime_p (f0 : MpzPtrT; f1 : INTEGER) : INTEGER;

<*EXTERNAL "__gmpz_realloc2" *>
PROCEDURE c_mpz_realloc2 (f0 : MpzPtrT; f1 : Word.T);

<*EXTERNAL "__gmpz_remove" *>
PROCEDURE c_mpz_remove (f0 : MpzPtrT; f1 : MpzPtrT; f2 : MpzPtrT) : Word.T;

<*EXTERNAL "__gmpz_root" *>
PROCEDURE c_mpz_root (f0 : MpzPtrT; f1 : MpzPtrT; f2 : Word.T) : INTEGER;

<*EXTERNAL "__gmpz_rootrem" *>
PROCEDURE c_mpz_rootrem (f0 : MpzPtrT; f1 : MpzPtrT; f2 : MpzPtrT; f3 : Word.T);

<*EXTERNAL "__gmpz_scan0" *>
PROCEDURE c_mpz_scan0 (f0 : MpzPtrT; f1 : Word.T) : Word.T;

<*EXTERNAL "__gmpz_scan1" *>
PROCEDURE c_mpz_scan1 (f0 : MpzPtrT; f1 : Word.T) : Word.T;

<*EXTERNAL "__gmpz_set" *>
PROCEDURE c_mpz_set (f0 : MpzPtrT; f1 : MpzPtrT);

<*EXTERNAL "__gmpz_set_d" *>
PROCEDURE c_mpz_set_d (f0 : MpzPtrT; f1 : LONGREAL);

<*EXTERNAL "__gmpz_set_si" *>
PROCEDURE c_mpz_set_si (f0 : MpzPtrT; f1 : INTEGER);

<*EXTERNAL "__gmpz_set_str" *>
PROCEDURE c_mpz_set_str (f0 : MpzPtrT; f1 : Ctypes.const_char_star; f2 : INTEGER) : INTEGER;

<*EXTERNAL "__gmpz_set_ui" *>
PROCEDURE c_mpz_set_ui (f0 : MpzPtrT; f1 : Word.T);

<*EXTERNAL "__gmpz_setbit" *>
PROCEDURE c_mpz_setbit (f0 : MpzPtrT; f1 : Word.T);

<*EXTERNAL "__gmpz_size" *>
PROCEDURE c_mpz_size (f0 : MpzPtrT) : CARDINAL;

<*EXTERNAL "__gmpz_sizeinbase" *>
PROCEDURE c_mpz_sizeinbase (f0 : MpzPtrT; f1 : INTEGER) : CARDINAL;

<*EXTERNAL "__gmpz_sqrt" *>
PROCEDURE c_mpz_sqrt (f0 : MpzPtrT; f1 : MpzPtrT);

<*EXTERNAL "__gmpz_sqrtrem" *>
PROCEDURE c_mpz_sqrtrem (f0 : MpzPtrT; f1 : MpzPtrT; f2 : MpzPtrT);

<*EXTERNAL "__gmpz_sub" *>
PROCEDURE c_mpz_sub (f0 : MpzPtrT; f1 : MpzPtrT; f2 : MpzPtrT);

<*EXTERNAL "__gmpz_sub_ui" *>
PROCEDURE c_mpz_sub_ui (f0 : MpzPtrT; f1 : MpzPtrT; f2 : Word.T);

<*EXTERNAL "__gmpz_ui_sub" *>
PROCEDURE c_mpz_ui_sub (f0 : MpzPtrT; f1 : Word.T; f2 : MpzPtrT);

<*EXTERNAL "__gmpz_submul" *>
PROCEDURE c_mpz_submul (f0 : MpzPtrT; f1 : MpzPtrT; f2 : MpzPtrT);

<*EXTERNAL "__gmpz_submul_ui" *>
PROCEDURE c_mpz_submul_ui (f0 : MpzPtrT; f1 : MpzPtrT; f2 : Word.T);

<*EXTERNAL "__gmpz_swap" *>
PROCEDURE c_mpz_swap (f0 : MpzPtrT; f1 : MpzPtrT);

<*EXTERNAL "__gmpz_tdiv_ui" *>
PROCEDURE c_mpz_tdiv_ui (f0 : MpzPtrT; f1 : Word.T) : Word.T;

<*EXTERNAL "__gmpz_tdiv_q" *>
PROCEDURE c_mpz_tdiv_q (f0 : MpzPtrT; f1 : MpzPtrT; f2 : MpzPtrT);

<*EXTERNAL "__gmpz_tdiv_q_2exp" *>
PROCEDURE c_mpz_tdiv_q_2exp (f0 : MpzPtrT; f1 : MpzPtrT; f2 : Word.T);

<*EXTERNAL "__gmpz_tdiv_q_ui" *>
PROCEDURE c_mpz_tdiv_q_ui (f0 : MpzPtrT; f1 : MpzPtrT; f2 : Word.T) : Word.T;

<*EXTERNAL "__gmpz_tdiv_qr" *>
PROCEDURE c_mpz_tdiv_qr (f0 : MpzPtrT; f1 : MpzPtrT; f2 : MpzPtrT; f3 : MpzPtrT);

<*EXTERNAL "__gmpz_tdiv_qr_ui" *>
PROCEDURE c_mpz_tdiv_qr_ui (f0 : MpzPtrT; f1 : MpzPtrT; f2 : MpzPtrT; f3 : Word.T) : Word.T;

<*EXTERNAL "__gmpz_tdiv_r" *>
PROCEDURE c_mpz_tdiv_r (f0 : MpzPtrT; f1 : MpzPtrT; f2 : MpzPtrT);

<*EXTERNAL "__gmpz_tdiv_r_2exp" *>
PROCEDURE c_mpz_tdiv_r_2exp (f0 : MpzPtrT; f1 : MpzPtrT; f2 : Word.T);

<*EXTERNAL "__gmpz_tdiv_r_ui" *>
PROCEDURE c_mpz_tdiv_r_ui (f0 : MpzPtrT; f1 : MpzPtrT; f2 : Word.T) : Word.T;

<*EXTERNAL "__gmpz_tstbit" *>
PROCEDURE c_mpz_tstbit (f0 : MpzPtrT; f1 : Word.T) : INTEGER;

<*EXTERNAL "__gmpz_ui_pow_ui" *>
PROCEDURE c_mpz_ui_pow_ui (f0 : MpzPtrT; f1 : Word.T; f2 : Word.T);

<*EXTERNAL "__gmpz_xor" *>
PROCEDURE c_mpz_xor (f0 : MpzPtrT; f1 : MpzPtrT; f2 : MpzPtrT);

<*EXTERNAL "__gmpf_set_default_prec" *>
PROCEDURE c_mpf_set_default_prec (f0 : Word.T);

END MpzP.

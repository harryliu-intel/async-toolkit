UNSAFE MODULE MpzOps;
IMPORT MpzRep;
IMPORT MpzP AS P;
IMPORT Word;
IMPORT M3toC;

PROCEDURE mpz_abs (f0 : T; f1 : T) =
  BEGIN
    P.c_mpz_abs(ADR(f0.val),ADR(f1.val))
  END mpz_abs;

PROCEDURE mpz_add (f0 : T; f1 : T; f2 : T) =
  BEGIN
    P.c_mpz_add(ADR(f0.val),ADR(f1.val),ADR(f2.val))
  END mpz_add;

PROCEDURE mpz_add_ui (f0 : T; f1 : T; f2 : Word.T) =
  BEGIN
    P.c_mpz_add_ui(ADR(f0.val),ADR(f1.val),f2)
  END mpz_add_ui;

PROCEDURE mpz_addmul (f0 : T; f1 : T; f2 : T) =
  BEGIN
    P.c_mpz_addmul(ADR(f0.val),ADR(f1.val),ADR(f2.val))
  END mpz_addmul;

PROCEDURE mpz_addmul_ui (f0 : T; f1 : T; f2 : Word.T) =
  BEGIN
    P.c_mpz_addmul_ui(ADR(f0.val),ADR(f1.val),f2)
  END mpz_addmul_ui;

PROCEDURE mpz_and (f0 : T; f1 : T; f2 : T) =
  BEGIN
    P.c_mpz_and(ADR(f0.val),ADR(f1.val),ADR(f2.val))
  END mpz_and;

PROCEDURE mpz_bin_ui (f0 : T; f1 : T; f2 : Word.T) =
  BEGIN
    P.c_mpz_bin_ui(ADR(f0.val),ADR(f1.val),f2)
  END mpz_bin_ui;

PROCEDURE mpz_bin_uiui (f0 : T; f1 : Word.T; f2 : Word.T) =
  BEGIN
    P.c_mpz_bin_uiui(ADR(f0.val),f1,f2)
  END mpz_bin_uiui;

PROCEDURE mpz_cdiv_q (f0 : T; f1 : T; f2 : T) =
  BEGIN
    P.c_mpz_cdiv_q(ADR(f0.val),ADR(f1.val),ADR(f2.val))
  END mpz_cdiv_q;

PROCEDURE mpz_cdiv_q_2exp (f0 : T; f1 : T; f2 : Word.T) =
  BEGIN
    P.c_mpz_cdiv_q_2exp(ADR(f0.val),ADR(f1.val),f2)
  END mpz_cdiv_q_2exp;

PROCEDURE mpz_cdiv_q_ui (f0 : T; f1 : T; f2 : Word.T) : Word.T =
  BEGIN
    RETURN P.c_mpz_cdiv_q_ui(ADR(f0.val),ADR(f1.val),f2)
  END mpz_cdiv_q_ui;

PROCEDURE mpz_cdiv_qr (f0 : T; f1 : T; f2 : T; f3 : T) =
  BEGIN
    P.c_mpz_cdiv_qr(ADR(f0.val),ADR(f1.val),ADR(f2.val),ADR(f3.val))
  END mpz_cdiv_qr;

PROCEDURE mpz_cdiv_qr_ui (f0 : T; f1 : T; f2 : T; f3 : Word.T) : Word.T =
  BEGIN
    RETURN P.c_mpz_cdiv_qr_ui(ADR(f0.val),ADR(f1.val),ADR(f2.val),f3)
  END mpz_cdiv_qr_ui;

PROCEDURE mpz_cdiv_r (f0 : T; f1 : T; f2 : T) =
  BEGIN
    P.c_mpz_cdiv_r(ADR(f0.val),ADR(f1.val),ADR(f2.val))
  END mpz_cdiv_r;

PROCEDURE mpz_cdiv_r_2exp (f0 : T; f1 : T; f2 : Word.T) =
  BEGIN
    P.c_mpz_cdiv_r_2exp(ADR(f0.val),ADR(f1.val),f2)
  END mpz_cdiv_r_2exp;

PROCEDURE mpz_cdiv_r_ui (f0 : T; f1 : T; f2 : Word.T) : Word.T =
  BEGIN
    RETURN P.c_mpz_cdiv_r_ui(ADR(f0.val),ADR(f1.val),f2)
  END mpz_cdiv_r_ui;

PROCEDURE mpz_cdiv_ui (f0 : T; f1 : Word.T) : Word.T =
  BEGIN
    RETURN P.c_mpz_cdiv_ui(ADR(f0.val),f1)
  END mpz_cdiv_ui;

PROCEDURE mpz_clear (f0 : T) =
  BEGIN
    P.c_mpz_clear(ADR(f0.val))
  END mpz_clear;

PROCEDURE mpz_clrbit (f0 : T; f1 : Word.T) =
  BEGIN
    P.c_mpz_clrbit(ADR(f0.val),f1)
  END mpz_clrbit;

PROCEDURE mpz_cmp (f0 : T; f1 : T) : INTEGER =
  BEGIN
    RETURN P.c_mpz_cmp(ADR(f0.val),ADR(f1.val))
  END mpz_cmp;

PROCEDURE mpz_cmp_d (f0 : T; f1 : LONGREAL) : INTEGER =
  BEGIN
    RETURN P.c_mpz_cmp_d(ADR(f0.val),f1)
  END mpz_cmp_d;

PROCEDURE mpz_cmpabs (f0 : T; f1 : T) : INTEGER =
  BEGIN
    RETURN P.c_mpz_cmpabs(ADR(f0.val),ADR(f1.val))
  END mpz_cmpabs;

PROCEDURE mpz_cmpabs_d (f0 : T; f1 : LONGREAL) : INTEGER =
  BEGIN
    RETURN P.c_mpz_cmpabs_d(ADR(f0.val),f1)
  END mpz_cmpabs_d;

PROCEDURE mpz_cmpabs_ui (f0 : T; f1 : Word.T) : INTEGER =
  BEGIN
    RETURN P.c_mpz_cmpabs_ui(ADR(f0.val),f1)
  END mpz_cmpabs_ui;

PROCEDURE mpz_com (f0 : T; f1 : T) =
  BEGIN
    P.c_mpz_com(ADR(f0.val),ADR(f1.val))
  END mpz_com;

PROCEDURE mpz_combit (f0 : T; f1 : Word.T) =
  BEGIN
    P.c_mpz_combit(ADR(f0.val),f1)
  END mpz_combit;

PROCEDURE mpz_congruent_p (f0 : T; f1 : T; f2 : T) : INTEGER =
  BEGIN
    RETURN P.c_mpz_congruent_p(ADR(f0.val),ADR(f1.val),ADR(f2.val))
  END mpz_congruent_p;

PROCEDURE mpz_congruent_2exp_p (f0 : T; f1 : T; f2 : Word.T) : INTEGER =
  BEGIN
    RETURN P.c_mpz_congruent_2exp_p(ADR(f0.val),ADR(f1.val),f2)
  END mpz_congruent_2exp_p;

PROCEDURE mpz_congruent_ui_p (f0 : T; f1 : Word.T; f2 : Word.T) : INTEGER =
  BEGIN
    RETURN P.c_mpz_congruent_ui_p(ADR(f0.val),f1,f2)
  END mpz_congruent_ui_p;

PROCEDURE mpz_divexact (f0 : T; f1 : T; f2 : T) =
  BEGIN
    P.c_mpz_divexact(ADR(f0.val),ADR(f1.val),ADR(f2.val))
  END mpz_divexact;

PROCEDURE mpz_divexact_ui (f0 : T; f1 : T; f2 : Word.T) =
  BEGIN
    P.c_mpz_divexact_ui(ADR(f0.val),ADR(f1.val),f2)
  END mpz_divexact_ui;

PROCEDURE mpz_divisible_p (f0 : T; f1 : T) : INTEGER =
  BEGIN
    RETURN P.c_mpz_divisible_p(ADR(f0.val),ADR(f1.val))
  END mpz_divisible_p;

PROCEDURE mpz_divisible_ui_p (f0 : T; f1 : Word.T) : INTEGER =
  BEGIN
    RETURN P.c_mpz_divisible_ui_p(ADR(f0.val),f1)
  END mpz_divisible_ui_p;

PROCEDURE mpz_divisible_2exp_p (f0 : T; f1 : Word.T) : INTEGER =
  BEGIN
    RETURN P.c_mpz_divisible_2exp_p(ADR(f0.val),f1)
  END mpz_divisible_2exp_p;

PROCEDURE mpz_dump (f0 : T) =
  BEGIN
    P.c_mpz_dump(ADR(f0.val))
  END mpz_dump;

PROCEDURE mpz_fac_ui (f0 : T; f1 : Word.T) =
  BEGIN
    P.c_mpz_fac_ui(ADR(f0.val),f1)
  END mpz_fac_ui;

PROCEDURE mpz_fdiv_q (f0 : T; f1 : T; f2 : T) =
  BEGIN
    P.c_mpz_fdiv_q(ADR(f0.val),ADR(f1.val),ADR(f2.val))
  END mpz_fdiv_q;

PROCEDURE mpz_fdiv_q_2exp (f0 : T; f1 : T; f2 : Word.T) =
  BEGIN
    P.c_mpz_fdiv_q_2exp(ADR(f0.val),ADR(f1.val),f2)
  END mpz_fdiv_q_2exp;

PROCEDURE mpz_fdiv_q_ui (f0 : T; f1 : T; f2 : Word.T) : Word.T =
  BEGIN
    RETURN P.c_mpz_fdiv_q_ui(ADR(f0.val),ADR(f1.val),f2)
  END mpz_fdiv_q_ui;

PROCEDURE mpz_fdiv_qr (f0 : T; f1 : T; f2 : T; f3 : T) =
  BEGIN
    P.c_mpz_fdiv_qr(ADR(f0.val),ADR(f1.val),ADR(f2.val),ADR(f3.val))
  END mpz_fdiv_qr;

PROCEDURE mpz_fdiv_qr_ui (f0 : T; f1 : T; f2 : T; f3 : Word.T) : Word.T =
  BEGIN
    RETURN P.c_mpz_fdiv_qr_ui(ADR(f0.val),ADR(f1.val),ADR(f2.val),f3)
  END mpz_fdiv_qr_ui;

PROCEDURE mpz_fdiv_r (f0 : T; f1 : T; f2 : T) =
  BEGIN
    P.c_mpz_fdiv_r(ADR(f0.val),ADR(f1.val),ADR(f2.val))
  END mpz_fdiv_r;

PROCEDURE mpz_fdiv_r_2exp (f0 : T; f1 : T; f2 : Word.T) =
  BEGIN
    P.c_mpz_fdiv_r_2exp(ADR(f0.val),ADR(f1.val),f2)
  END mpz_fdiv_r_2exp;

PROCEDURE mpz_fdiv_r_ui (f0 : T; f1 : T; f2 : Word.T) : Word.T =
  BEGIN
    RETURN P.c_mpz_fdiv_r_ui(ADR(f0.val),ADR(f1.val),f2)
  END mpz_fdiv_r_ui;

PROCEDURE mpz_fdiv_ui (f0 : T; f1 : Word.T) : Word.T =
  BEGIN
    RETURN P.c_mpz_fdiv_ui(ADR(f0.val),f1)
  END mpz_fdiv_ui;

PROCEDURE mpz_fib_ui (f0 : T; f1 : Word.T) =
  BEGIN
    P.c_mpz_fib_ui(ADR(f0.val),f1)
  END mpz_fib_ui;

PROCEDURE mpz_fib2_ui (f0 : T; f1 : T; f2 : Word.T) =
  BEGIN
    P.c_mpz_fib2_ui(ADR(f0.val),ADR(f1.val),f2)
  END mpz_fib2_ui;

PROCEDURE mpz_fits_sint_p (f0 : T) : INTEGER =
  BEGIN
    RETURN P.c_mpz_fits_sint_p(ADR(f0.val))
  END mpz_fits_sint_p;

PROCEDURE mpz_fits_slong_p (f0 : T) : INTEGER =
  BEGIN
    RETURN P.c_mpz_fits_slong_p(ADR(f0.val))
  END mpz_fits_slong_p;

PROCEDURE mpz_fits_sshort_p (f0 : T) : INTEGER =
  BEGIN
    RETURN P.c_mpz_fits_sshort_p(ADR(f0.val))
  END mpz_fits_sshort_p;

PROCEDURE mpz_fits_uint_p (f0 : T) : INTEGER =
  BEGIN
    RETURN P.c_mpz_fits_uint_p(ADR(f0.val))
  END mpz_fits_uint_p;

PROCEDURE mpz_fits_ulong_p (f0 : T) : INTEGER =
  BEGIN
    RETURN P.c_mpz_fits_ulong_p(ADR(f0.val))
  END mpz_fits_ulong_p;

PROCEDURE mpz_fits_ushort_p (f0 : T) : INTEGER =
  BEGIN
    RETURN P.c_mpz_fits_ushort_p(ADR(f0.val))
  END mpz_fits_ushort_p;

PROCEDURE mpz_gcd (f0 : T; f1 : T; f2 : T) =
  BEGIN
    P.c_mpz_gcd(ADR(f0.val),ADR(f1.val),ADR(f2.val))
  END mpz_gcd;

PROCEDURE mpz_gcd_ui (f0 : T; f1 : T; f2 : Word.T) : Word.T =
  BEGIN
    RETURN P.c_mpz_gcd_ui(ADR(f0.val),ADR(f1.val),f2)
  END mpz_gcd_ui;

PROCEDURE mpz_gcdext (f0 : T; f1 : T; f2 : T; f3 : T; f4 : T) =
  BEGIN
    P.c_mpz_gcdext(ADR(f0.val),ADR(f1.val),ADR(f2.val),ADR(f3.val),ADR(f4.val))
  END mpz_gcdext;

PROCEDURE mpz_get_d (f0 : T) : LONGREAL =
  BEGIN
    RETURN P.c_mpz_get_d(ADR(f0.val))
  END mpz_get_d;

PROCEDURE mpz_get_ui (f0 : T) : Word.T =
  BEGIN
    RETURN P.c_mpz_get_ui(ADR(f0.val))
  END mpz_get_ui;

PROCEDURE mpz_hamdist (f0 : T; f1 : T) : Word.T =
  BEGIN
    RETURN P.c_mpz_hamdist(ADR(f0.val),ADR(f1.val))
  END mpz_hamdist;

PROCEDURE mpz_init (f0 : T) =
  BEGIN
    P.c_mpz_init(ADR(f0.val))
  END mpz_init;

PROCEDURE mpz_init2 (f0 : T; f1 : Word.T) =
  BEGIN
    P.c_mpz_init2(ADR(f0.val),f1)
  END mpz_init2;

PROCEDURE mpz_init_set (f0 : T; f1 : T) =
  BEGIN
    P.c_mpz_init_set(ADR(f0.val),ADR(f1.val))
  END mpz_init_set;

PROCEDURE mpz_init_set_d (f0 : T; f1 : LONGREAL) =
  BEGIN
    P.c_mpz_init_set_d(ADR(f0.val),f1)
  END mpz_init_set_d;

PROCEDURE mpz_init_set_si (f0 : T; f1 : INTEGER) =
  BEGIN
    P.c_mpz_init_set_si(ADR(f0.val),f1)
  END mpz_init_set_si;

PROCEDURE mpz_init_set_str (f0 : T; f1 : TEXT; f2 : INTEGER) : INTEGER =
  BEGIN
    RETURN P.c_mpz_init_set_str(ADR(f0.val),M3toC.CopyTtoS(f1),f2)
  END mpz_init_set_str;

PROCEDURE mpz_init_set_ui (f0 : T; f1 : Word.T) =
  BEGIN
    P.c_mpz_init_set_ui(ADR(f0.val),f1)
  END mpz_init_set_ui;

PROCEDURE mpz_invert (f0 : T; f1 : T; f2 : T) : INTEGER =
  BEGIN
    RETURN P.c_mpz_invert(ADR(f0.val),ADR(f1.val),ADR(f2.val))
  END mpz_invert;

PROCEDURE mpz_ior (f0 : T; f1 : T; f2 : T) =
  BEGIN
    P.c_mpz_ior(ADR(f0.val),ADR(f1.val),ADR(f2.val))
  END mpz_ior;

PROCEDURE mpz_jacobi (f0 : T; f1 : T) : INTEGER =
  BEGIN
    RETURN P.c_mpz_jacobi(ADR(f0.val),ADR(f1.val))
  END mpz_jacobi;

PROCEDURE mpz_kronecker_ui (f0 : T; f1 : Word.T) : INTEGER =
  BEGIN
    RETURN P.c_mpz_kronecker_ui(ADR(f0.val),f1)
  END mpz_kronecker_ui;

PROCEDURE mpz_ui_kronecker (f0 : Word.T; f1 : T) : INTEGER =
  BEGIN
    RETURN P.c_mpz_ui_kronecker(f0,ADR(f1.val))
  END mpz_ui_kronecker;

PROCEDURE mpz_lcm (f0 : T; f1 : T; f2 : T) =
  BEGIN
    P.c_mpz_lcm(ADR(f0.val),ADR(f1.val),ADR(f2.val))
  END mpz_lcm;

PROCEDURE mpz_lcm_ui (f0 : T; f1 : T; f2 : Word.T) =
  BEGIN
    P.c_mpz_lcm_ui(ADR(f0.val),ADR(f1.val),f2)
  END mpz_lcm_ui;

PROCEDURE mpz_lucnum_ui (f0 : T; f1 : Word.T) =
  BEGIN
    P.c_mpz_lucnum_ui(ADR(f0.val),f1)
  END mpz_lucnum_ui;

PROCEDURE mpz_lucnum2_ui (f0 : T; f1 : T; f2 : Word.T) =
  BEGIN
    P.c_mpz_lucnum2_ui(ADR(f0.val),ADR(f1.val),f2)
  END mpz_lucnum2_ui;

PROCEDURE mpz_millerrabin (f0 : T; f1 : INTEGER) : INTEGER =
  BEGIN
    RETURN P.c_mpz_millerrabin(ADR(f0.val),f1)
  END mpz_millerrabin;

PROCEDURE mpz_mod (f0 : T; f1 : T; f2 : T) =
  BEGIN
    P.c_mpz_mod(ADR(f0.val),ADR(f1.val),ADR(f2.val))
  END mpz_mod;

PROCEDURE mpz_mul (f0 : T; f1 : T; f2 : T) =
  BEGIN
    P.c_mpz_mul(ADR(f0.val),ADR(f1.val),ADR(f2.val))
  END mpz_mul;

PROCEDURE mpz_mul_2exp (f0 : T; f1 : T; f2 : Word.T) =
  BEGIN
    P.c_mpz_mul_2exp(ADR(f0.val),ADR(f1.val),f2)
  END mpz_mul_2exp;

PROCEDURE mpz_mul_ui (f0 : T; f1 : T; f2 : Word.T) =
  BEGIN
    P.c_mpz_mul_ui(ADR(f0.val),ADR(f1.val),f2)
  END mpz_mul_ui;

PROCEDURE mpz_neg (f0 : T; f1 : T) =
  BEGIN
    P.c_mpz_neg(ADR(f0.val),ADR(f1.val))
  END mpz_neg;

PROCEDURE mpz_nextprime (f0 : T; f1 : T) =
  BEGIN
    P.c_mpz_nextprime(ADR(f0.val),ADR(f1.val))
  END mpz_nextprime;

PROCEDURE mpz_perfect_power_p (f0 : T) : INTEGER =
  BEGIN
    RETURN P.c_mpz_perfect_power_p(ADR(f0.val))
  END mpz_perfect_power_p;

PROCEDURE mpz_perfect_square_p (f0 : T) : INTEGER =
  BEGIN
    RETURN P.c_mpz_perfect_square_p(ADR(f0.val))
  END mpz_perfect_square_p;

PROCEDURE mpz_popcount (f0 : T) : Word.T =
  BEGIN
    RETURN P.c_mpz_popcount(ADR(f0.val))
  END mpz_popcount;

PROCEDURE mpz_pow_ui (f0 : T; f1 : T; f2 : Word.T) =
  BEGIN
    P.c_mpz_pow_ui(ADR(f0.val),ADR(f1.val),f2)
  END mpz_pow_ui;

PROCEDURE mpz_powm (f0 : T; f1 : T; f2 : T; f3 : T) =
  BEGIN
    P.c_mpz_powm(ADR(f0.val),ADR(f1.val),ADR(f2.val),ADR(f3.val))
  END mpz_powm;

PROCEDURE mpz_powm_ui (f0 : T; f1 : T; f2 : Word.T; f3 : T) =
  BEGIN
    P.c_mpz_powm_ui(ADR(f0.val),ADR(f1.val),f2,ADR(f3.val))
  END mpz_powm_ui;

PROCEDURE mpz_probab_prime_p (f0 : T; f1 : INTEGER) : INTEGER =
  BEGIN
    RETURN P.c_mpz_probab_prime_p(ADR(f0.val),f1)
  END mpz_probab_prime_p;

PROCEDURE mpz_realloc2 (f0 : T; f1 : Word.T) =
  BEGIN
    P.c_mpz_realloc2(ADR(f0.val),f1)
  END mpz_realloc2;

PROCEDURE mpz_remove (f0 : T; f1 : T; f2 : T) : Word.T =
  BEGIN
    RETURN P.c_mpz_remove(ADR(f0.val),ADR(f1.val),ADR(f2.val))
  END mpz_remove;

PROCEDURE mpz_root (f0 : T; f1 : T; f2 : Word.T) : INTEGER =
  BEGIN
    RETURN P.c_mpz_root(ADR(f0.val),ADR(f1.val),f2)
  END mpz_root;

PROCEDURE mpz_rootrem (f0 : T; f1 : T; f2 : T; f3 : Word.T) =
  BEGIN
    P.c_mpz_rootrem(ADR(f0.val),ADR(f1.val),ADR(f2.val),f3)
  END mpz_rootrem;

PROCEDURE mpz_scan0 (f0 : T; f1 : Word.T) : Word.T =
  BEGIN
    RETURN P.c_mpz_scan0(ADR(f0.val),f1)
  END mpz_scan0;

PROCEDURE mpz_scan1 (f0 : T; f1 : Word.T) : Word.T =
  BEGIN
    RETURN P.c_mpz_scan1(ADR(f0.val),f1)
  END mpz_scan1;

PROCEDURE mpz_set (f0 : T; f1 : T) =
  BEGIN
    P.c_mpz_set(ADR(f0.val),ADR(f1.val))
  END mpz_set;

PROCEDURE mpz_set_d (f0 : T; f1 : LONGREAL) =
  BEGIN
    P.c_mpz_set_d(ADR(f0.val),f1)
  END mpz_set_d;

PROCEDURE mpz_set_si (f0 : T; f1 : INTEGER) =
  BEGIN
    P.c_mpz_set_si(ADR(f0.val),f1)
  END mpz_set_si;

PROCEDURE mpz_set_str (f0 : T; f1 : TEXT; f2 : INTEGER) : INTEGER =
  BEGIN
    RETURN P.c_mpz_set_str(ADR(f0.val),M3toC.CopyTtoS(f1),f2)
  END mpz_set_str;

PROCEDURE mpz_set_ui (f0 : T; f1 : Word.T) =
  BEGIN
    P.c_mpz_set_ui(ADR(f0.val),f1)
  END mpz_set_ui;

PROCEDURE mpz_setbit (f0 : T; f1 : Word.T) =
  BEGIN
    P.c_mpz_setbit(ADR(f0.val),f1)
  END mpz_setbit;

PROCEDURE mpz_size (f0 : T) : CARDINAL =
  BEGIN
    RETURN P.c_mpz_size(ADR(f0.val))
  END mpz_size;

PROCEDURE mpz_sizeinbase (f0 : T; f1 : INTEGER) : CARDINAL =
  BEGIN
    RETURN P.c_mpz_sizeinbase(ADR(f0.val),f1)
  END mpz_sizeinbase;

PROCEDURE mpz_sqrt (f0 : T; f1 : T) =
  BEGIN
    P.c_mpz_sqrt(ADR(f0.val),ADR(f1.val))
  END mpz_sqrt;

PROCEDURE mpz_sqrtrem (f0 : T; f1 : T; f2 : T) =
  BEGIN
    P.c_mpz_sqrtrem(ADR(f0.val),ADR(f1.val),ADR(f2.val))
  END mpz_sqrtrem;

PROCEDURE mpz_sub (f0 : T; f1 : T; f2 : T) =
  BEGIN
    P.c_mpz_sub(ADR(f0.val),ADR(f1.val),ADR(f2.val))
  END mpz_sub;

PROCEDURE mpz_sub_ui (f0 : T; f1 : T; f2 : Word.T) =
  BEGIN
    P.c_mpz_sub_ui(ADR(f0.val),ADR(f1.val),f2)
  END mpz_sub_ui;

PROCEDURE mpz_ui_sub (f0 : T; f1 : Word.T; f2 : T) =
  BEGIN
    P.c_mpz_ui_sub(ADR(f0.val),f1,ADR(f2.val))
  END mpz_ui_sub;

PROCEDURE mpz_submul (f0 : T; f1 : T; f2 : T) =
  BEGIN
    P.c_mpz_submul(ADR(f0.val),ADR(f1.val),ADR(f2.val))
  END mpz_submul;

PROCEDURE mpz_submul_ui (f0 : T; f1 : T; f2 : Word.T) =
  BEGIN
    P.c_mpz_submul_ui(ADR(f0.val),ADR(f1.val),f2)
  END mpz_submul_ui;

PROCEDURE mpz_swap (f0 : T; f1 : T) =
  BEGIN
    P.c_mpz_swap(ADR(f0.val),ADR(f1.val))
  END mpz_swap;

PROCEDURE mpz_tdiv_ui (f0 : T; f1 : Word.T) : Word.T =
  BEGIN
    RETURN P.c_mpz_tdiv_ui(ADR(f0.val),f1)
  END mpz_tdiv_ui;

PROCEDURE mpz_tdiv_q (f0 : T; f1 : T; f2 : T) =
  BEGIN
    P.c_mpz_tdiv_q(ADR(f0.val),ADR(f1.val),ADR(f2.val))
  END mpz_tdiv_q;

PROCEDURE mpz_tdiv_q_2exp (f0 : T; f1 : T; f2 : Word.T) =
  BEGIN
    P.c_mpz_tdiv_q_2exp(ADR(f0.val),ADR(f1.val),f2)
  END mpz_tdiv_q_2exp;

PROCEDURE mpz_tdiv_q_ui (f0 : T; f1 : T; f2 : Word.T) : Word.T =
  BEGIN
    RETURN P.c_mpz_tdiv_q_ui(ADR(f0.val),ADR(f1.val),f2)
  END mpz_tdiv_q_ui;

PROCEDURE mpz_tdiv_qr (f0 : T; f1 : T; f2 : T; f3 : T) =
  BEGIN
    P.c_mpz_tdiv_qr(ADR(f0.val),ADR(f1.val),ADR(f2.val),ADR(f3.val))
  END mpz_tdiv_qr;

PROCEDURE mpz_tdiv_qr_ui (f0 : T; f1 : T; f2 : T; f3 : Word.T) : Word.T =
  BEGIN
    RETURN P.c_mpz_tdiv_qr_ui(ADR(f0.val),ADR(f1.val),ADR(f2.val),f3)
  END mpz_tdiv_qr_ui;

PROCEDURE mpz_tdiv_r (f0 : T; f1 : T; f2 : T) =
  BEGIN
    P.c_mpz_tdiv_r(ADR(f0.val),ADR(f1.val),ADR(f2.val))
  END mpz_tdiv_r;

PROCEDURE mpz_tdiv_r_2exp (f0 : T; f1 : T; f2 : Word.T) =
  BEGIN
    P.c_mpz_tdiv_r_2exp(ADR(f0.val),ADR(f1.val),f2)
  END mpz_tdiv_r_2exp;

PROCEDURE mpz_tdiv_r_ui (f0 : T; f1 : T; f2 : Word.T) : Word.T =
  BEGIN
    RETURN P.c_mpz_tdiv_r_ui(ADR(f0.val),ADR(f1.val),f2)
  END mpz_tdiv_r_ui;

PROCEDURE mpz_tstbit (f0 : T; f1 : Word.T) : INTEGER =
  BEGIN
    RETURN P.c_mpz_tstbit(ADR(f0.val),f1)
  END mpz_tstbit;

PROCEDURE mpz_ui_pow_ui (f0 : T; f1 : Word.T; f2 : Word.T) =
  BEGIN
    P.c_mpz_ui_pow_ui(ADR(f0.val),f1,f2)
  END mpz_ui_pow_ui;

PROCEDURE mpz_xor (f0 : T; f1 : T; f2 : T) =
  BEGIN
    P.c_mpz_xor(ADR(f0.val),ADR(f1.val),ADR(f2.val))
  END mpz_xor;

PROCEDURE mpf_set_default_prec (f0 : Word.T) =
  BEGIN
    P.c_mpf_set_default_prec(f0)
  END mpf_set_default_prec;

BEGIN END MpzOps.

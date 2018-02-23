#include <math.h>
#include <HsFFI.h>

HsDouble ld_get_d(const long double *a) { return *a; }
HsInt    ld_get_i(const long double *a) { return *a; }
void     ld_set_d(long double *r, HsDouble b) { *r = b; }
void     ld_set_i(long double *r, HsInt b) { *r = b; }

void ld_add(long double *r, const long double *a, const long double *b) { *r = *a + *b; }
void ld_sub(long double *r, const long double *a, const long double *b) { *r = *a - *b; }
void ld_mul(long double *r, const long double *a, const long double *b) { *r = *a * *b; }
void ld_div(long double *r, const long double *a, const long double *b) { *r = *a / *b; }
void ld_pow(long double *r, const long double *a, const long double *b) { *r = powl(*a, *b); }
void ld_atan2(long double *r, const long double *a, const long double *b) { *r = atan2l(*a, *b); }
void ld_min(long double *r, const long double *a, const long double *b) { *r = fminl(*a, *b); }
void ld_max(long double *r, const long double *a, const long double *b) { *r = fmaxl(*a, *b); }

int ld_lt(const long double *a, const long double *b) { return *a <  *b; }
int ld_le(const long double *a, const long double *b) { return *a <= *b; }
int ld_eq(const long double *a, const long double *b) { return *a == *b; }
int ld_ne(const long double *a, const long double *b) { return *a != *b; }
int ld_ge(const long double *a, const long double *b) { return *a >= *b; }
int ld_gt(const long double *a, const long double *b) { return *a >  *b; }

void ld_abs(long double *r, const long double *a) { *r = fabsl(*a); }
void ld_sgn(long double *r, const long double *a) { *r = (*a > 0) - (0 > *a); }
void ld_neg(long double *r, const long double *a) { *r = - *a; }
void ld_recip(long double *r, const long double *a) { *r = 1.0L / *a; }
void ld_sqrt(long double *r, const long double *a) { *r = sqrtl(*a); }
void ld_exp(long double *r, const long double *a) { *r = expl(*a); }
void ld_log(long double *r, const long double *a) { *r = logl(*a); }
void ld_sin(long double *r, const long double *a) { *r = sinl(*a); }
void ld_cos(long double *r, const long double *a) { *r = cosl(*a); }
void ld_tan(long double *r, const long double *a) { *r = tanl(*a); }
void ld_sinh(long double *r, const long double *a) { *r = sinhl(*a); }
void ld_cosh(long double *r, const long double *a) { *r = coshl(*a); }
void ld_tanh(long double *r, const long double *a) { *r = tanhl(*a); }
void ld_asin(long double *r, const long double *a) { *r = asinl(*a); }
void ld_acos(long double *r, const long double *a) { *r = acosl(*a); }
void ld_atan(long double *r, const long double *a) { *r = atanl(*a); }
void ld_asinh(long double *r, const long double *a) { *r = asinhl(*a); }
void ld_acosh(long double *r, const long double *a) { *r = acoshl(*a); }
void ld_atanh(long double *r, const long double *a) { *r = atanhl(*a); }
void ld_floor(long double *r, const long double *a) { *r = floorl(*a); }
void ld_ceil(long double *r, const long double *a) { *r = ceill(*a); }
void ld_round(long double *r, const long double *a) { *r = roundl(*a); }
void ld_trunc(long double *r, const long double *a) { *r = truncl(*a); }

int ld_isnan(const long double *a) { return fpclassify(*a) == FP_NAN; }
int ld_isinf(const long double *a) { return fpclassify(*a) == FP_INFINITE; }
int ld_isdenorm(const long double *a) { return fpclassify(*a) == FP_SUBNORMAL; }
int ld_isnegzero(const long double *a) { return fpclassify(*a) == FP_ZERO && signbit(*a); }

void ld_ldexp(long double *r, const long double *a, int b) { *r = ldexpl(*a, b); }
void ld_frexp(long double *r, const long double *a, int *b) { *r = frexpl(*a, b); }

void ld_pi(long double *r) { *r = 3.141592653589793238462643383279502884L; }

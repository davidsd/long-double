#include <math.h>
#include <stdint.h>
#include <HsFFI.h>

typedef union
{
  long double ld;
  uint64_t u[2];
} ld128;

HsDouble ld128_get_d(const ld128 *a) { return a->ld; }
HsInt    ld128_get_i(const ld128 *a) { return a->ld; }
void     ld128_set_d(ld128 *r, HsDouble b) { r->ld = b; }
void     ld128_set_i(ld128 *r, HsInt b) { r->ld = b; }

void ld128_add(ld128 *r, const ld128 *a, const ld128 *b) { r->ld = a->ld + b->ld; }
void ld128_sub(ld128 *r, const ld128 *a, const ld128 *b) { r->ld = a->ld - b->ld; }
void ld128_mul(ld128 *r, const ld128 *a, const ld128 *b) { r->ld = a->ld * b->ld; }
void ld128_div(ld128 *r, const ld128 *a, const ld128 *b) { r->ld = a->ld / b->ld; }
void ld128_pow(ld128 *r, const ld128 *a, const ld128 *b) { r->ld = powl(a->ld, b->ld); }
void ld128_atan2(ld128 *r, const ld128 *a, const ld128 *b) { r->ld = atan2l(a->ld, b->ld); }
void ld128_min(ld128 *r, const ld128 *a, const ld128 *b) { r->ld = fminl(a->ld, b->ld); }
void ld128_max(ld128 *r, const ld128 *a, const ld128 *b) { r->ld = fmaxl(a->ld, b->ld); }

int ld128_lt(const ld128 *a, const ld128 *b) { return a->ld <  b->ld; }
int ld128_le(const ld128 *a, const ld128 *b) { return a->ld <= b->ld; }
int ld128_eq(const ld128 *a, const ld128 *b) { return a->ld == b->ld; }
int ld128_ne(const ld128 *a, const ld128 *b) { return a->ld != b->ld; }
int ld128_ge(const ld128 *a, const ld128 *b) { return a->ld >= b->ld; }
int ld128_gt(const ld128 *a, const ld128 *b) { return a->ld >  b->ld; }

void ld128_abs(ld128 *r, const ld128 *a) { r->ld = fabsl(a->ld); }
void ld128_sgn(ld128 *r, const ld128 *a) { r->ld = (a->ld > 0) - (0 > a->ld); }
void ld128_neg(ld128 *r, const ld128 *a) { r->ld = - a->ld; }
void ld128_recip(ld128 *r, const ld128 *a) { r->ld = 1.0L / a->ld; }
void ld128_sqrt(ld128 *r, const ld128 *a) { r->ld = sqrtl(a->ld); }
void ld128_exp(ld128 *r, const ld128 *a) { r->ld = expl(a->ld); }
void ld128_log(ld128 *r, const ld128 *a) { r->ld = logl(a->ld); }
void ld128_sin(ld128 *r, const ld128 *a) { r->ld = sinl(a->ld); }
void ld128_cos(ld128 *r, const ld128 *a) { r->ld = cosl(a->ld); }
void ld128_tan(ld128 *r, const ld128 *a) { r->ld = tanl(a->ld); }
void ld128_sinh(ld128 *r, const ld128 *a) { r->ld = sinhl(a->ld); }
void ld128_cosh(ld128 *r, const ld128 *a) { r->ld = coshl(a->ld); }
void ld128_tanh(ld128 *r, const ld128 *a) { r->ld = tanhl(a->ld); }
void ld128_asin(ld128 *r, const ld128 *a) { r->ld = asinl(a->ld); }
void ld128_acos(ld128 *r, const ld128 *a) { r->ld = acosl(a->ld); }
void ld128_atan(ld128 *r, const ld128 *a) { r->ld = atanl(a->ld); }
void ld128_asinh(ld128 *r, const ld128 *a) { r->ld = asinhl(a->ld); }
void ld128_acosh(ld128 *r, const ld128 *a) { r->ld = acoshl(a->ld); }
void ld128_atanh(ld128 *r, const ld128 *a) { r->ld = atanhl(a->ld); }
void ld128_floor(ld128 *r, const ld128 *a) { r->ld = floorl(a->ld); }
void ld128_ceil(ld128 *r, const ld128 *a) { r->ld = ceill(a->ld); }
void ld128_round(ld128 *r, const ld128 *a) { r->ld = roundl(a->ld); }
void ld128_trunc(ld128 *r, const ld128 *a) { r->ld = truncl(a->ld); }

int ld128_isnan(const ld128 *a) { return fpclassify(a->ld) == FP_NAN; }
int ld128_isinf(const ld128 *a) { return fpclassify(a->ld) == FP_INFINITE; }
int ld128_isdenorm(const ld128 *a) { return fpclassify(a->ld) == FP_SUBNORMAL; }
int ld128_isnegzero(const ld128 *a) { return fpclassify(a->ld) == FP_ZERO && signbit(a->ld); }

void ld128_ldexp(ld128 *r, const ld128 *a, int b) { r->ld = ldexpl(a->ld, b); }
void ld128_frexp(ld128 *r, const ld128 *a, int *b) { r->ld = frexpl(a->ld, b); }

void ld128_pi(ld128 *r) { r->ld = 3.141592653589793238462643383279502884L; }

#ifndef PP_GINAC_H
#define PP_GINAC_H

#include <ginac/ginac.h>

#ifdef __cplusplus
extern "C" {
#endif

GiNaC::symbol *ginac_symbol();
GiNaC::ex *ginac_ex_new_int(int i);
GiNaC::ex *ginac_ex_new_symbol(GiNaC::symbol *s);
GiNaC::ex *ginac_ex_new_x();
void ginac_ex_free(GiNaC::ex *e);
void ginac_ex_print(GiNaC::ex *e);
GiNaC::ex *ginac_abs(GiNaC::ex *e);
GiNaC::ex *ginac_signum(GiNaC::ex *e);
GiNaC::ex *ginac_neg(GiNaC::ex *e);
GiNaC::ex *ginac_add(GiNaC::ex *e_1, GiNaC::ex *e_2);
GiNaC::ex *ginac_mul(GiNaC::ex *e_1, GiNaC::ex *e_2);
GiNaC::ex *ginac_div(GiNaC::ex *e_1, GiNaC::ex *e_2);
GiNaC::ex *ginac_pow(GiNaC::ex *e_1, GiNaC::ex *e_2);
GiNaC::ex *ginac_diff(GiNaC::ex *e);
GiNaC::ex *ginac_factorial(int n, GiNaC::ex *e);
GiNaC::ex *ginac_sqrt(GiNaC::ex *e);
GiNaC::ex *ginac_subs(int i, GiNaC::ex *e);
bool ginac_is_numeric(GiNaC::ex *e);
double ginac_ex_to_double(GiNaC::ex *e);
int ginac_ex_to_int(GiNaC::ex *e);
char *ginac_ex_to_str(GiNaC::ex *e);

#ifdef __cplusplus
}
#endif

#endif // PP_GINAC_H

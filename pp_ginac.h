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
GiNaC::ex *ginac_add(GiNaC::ex *e_1, GiNaC::ex *e_2);

#ifdef __cplusplus
}
#endif

#endif // PP_GINAC_H

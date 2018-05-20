#ifndef PP_GINAC_H
#define PP_GINAC_H

#include <ginac/ginac.h>

#ifdef __cplusplus
extern "C" {
#endif

GiNaC::symbol *ginac_symbol();
GiNaC::ex *ginac_ex_new_symbol(GiNaC::symbol *s);
GiNaC::ex *ginac_ex_new_x();
void ginac_ex_free(GiNaC::ex *e);
void ginac_ex_print(GiNaC::ex *e);

#ifdef __cplusplus
}
#endif

#endif // PP_GINAC_H

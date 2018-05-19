#ifndef PP_GINAC_H
#define PP_GINAC_H

#include <ginac/ginac.h>

#ifdef __cplusplus
extern "C" {
#endif

GiNaC::symbol *ginac_symbol();

#ifdef __cplusplus
}
#endif

#endif // PP_GINAC_H


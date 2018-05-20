#include <iostream>
#include "pp_ginac.h"

using namespace GiNaC;

symbol x("x");

symbol *ginac_symbol()
{
    return &x;
}

ex *ginac_ex_new_symbol(symbol *s)
{
    return new ex(*s);
}

ex *ginac_ex_new_x()
{
    return new ex(x);
}

void ginac_ex_free(ex *e)
{
    delete e;
}

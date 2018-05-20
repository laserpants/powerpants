#include <iostream>
#include "pp_ginac.h"

using namespace GiNaC;

symbol x("x");

symbol *ginac_symbol()
{
    return &x;
}

ex *ginac_ex_new_int(int i)
{
    return new ex(i);
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

void ginac_ex_print(ex *e)
{
    std::cout << *e << std::endl;
}

ex *ginac_add(ex *e_1, ex *e_2)
{
    return new ex(add(*e_1, *e_2));
}

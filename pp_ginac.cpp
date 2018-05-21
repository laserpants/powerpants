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

ex *ginac_abs(ex *e)
{
    return new ex(abs(*e));
}

ex *ginac_signum(ex *e)
{
    if (*e > 0) {
        return new ex(1);
    } else if (*e < 0) {
        return new ex(-1);
    } else {
        return new ex(0);
    }
}

ex *ginac_neg(ex *e)
{
    return new ex(mul(-1, *e));
}

ex *ginac_add(ex *e_1, ex *e_2)
{
    return new ex(add(*e_1, *e_2));
}

ex *ginac_mul(ex *e_1, ex *e_2)
{
    return new ex(mul(*e_1, *e_2));
}

ex *ginac_div(ex *e_1, ex *e_2)
{
    return new ex(mul(*e_1, pow(*e_2, -1)));
}

ex *ginac_pow(ex *e_1, ex *e_2)
{
    return new ex(pow(*e_1, *e_2));
}

ex *ginac_diff(ex *e)
{
    return new ex(e->diff(x));
}

GiNaC::ex *ginac_factorial(int n, GiNaC::ex *e)
{
    return new ex(factorial(n));
}

ex *ginac_sqrt(ex *e)
{
    return new ex(sqrt(*e));
}

GiNaC::ex *ginac_subs(int i, GiNaC::ex *e)
{
    return new ex(e->subs(x == i));
}

bool ginac_is_numeric(ex *e)
{
    return is_a<numeric>(*e);
}

double ginac_ex_to_double(GiNaC::ex *e)
{
    return ex_to<numeric>(*e).to_double();
}

int ginac_ex_to_int(GiNaC::ex *e)
{
    return ex_to<numeric>(*e).to_int();
}

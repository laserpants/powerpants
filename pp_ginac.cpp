#include <iostream>
#include "pp_ginac.h"

using namespace GiNaC;

symbol *ginac_symbol()
{
    static symbol *x = new symbol;
    return x;
}

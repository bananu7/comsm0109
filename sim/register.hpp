#ifndef SIM_REG_HPP_INCLUDED
#define SIM_REG_HPP_INCLUDED

#include <iosfwd>

enum class reg {
    zero, one, pc, g1, g2, g3, g4, g5
};
std::ostream& operator<<(std::ostream& out, const reg& rhs);

#endif

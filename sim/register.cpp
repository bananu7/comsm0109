#include <register.hpp>
#include <iostream>

std::ostream& operator<<(std::ostream& out, const reg& rhs) {
    switch(rhs) {
    case reg::zero: out << "zero"; return out;
    case reg::one: out << "one"; return out;
    case reg::pc: out << "pc"; return out;
    case reg::g1: out << "g1"; return out;
    case reg::g2: out << "g2"; return out;
    case reg::g3: out << "g3"; return out;
    case reg::g4: out << "g4"; return out;
    case reg::g5: out << "g5"; return out;
    default: out << "???"; return out;
    }
}

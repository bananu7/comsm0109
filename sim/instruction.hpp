#ifndef SIM_INSTRUCTION_HPP_INCLUDED
#define SIM_INSTRUCTION_HPP_INCLUDED

#include <register.hpp>
#include <cstdint>
#include <variant>
#include <iosfwd>

struct binop {
    reg lhs;
    reg rhs;
    reg dst;
};

struct add : public binop {
    static constexpr const int opcode = 0;
    static constexpr const char* name = "add";
};
struct sub : public binop {
    static constexpr const int opcode = 1;
    static constexpr const char* name = "sub";
};
struct mul : public binop {
    static constexpr const int opcode = 2;
    static constexpr const char* name = "mul";
};
struct euc {
    static constexpr const int opcode = 3;
    static constexpr const char* name = "euc";
    reg lhs;
    reg rhs;
    reg dst_quo;
    reg dst_rem;
};
struct lls : public binop {
    static constexpr const int opcode = 4;
    static constexpr const char* name = "lls";
};
struct rls : public binop {
    static constexpr const int opcode = 5;
    static constexpr const char* name = "rls";
};
struct myand : public binop {
    static constexpr const int opcode = 6;
    static constexpr const char* name = "myand";
};
struct myor : public binop {
    static constexpr const int opcode = 7;
    static constexpr const char* name = "myor";
};
struct eq : public binop {
    static constexpr const int opcode = 8;
    static constexpr const char* name = "eq";
};
struct gt : public binop {
    static constexpr const int opcode = 9;
    static constexpr const char* name = "gt";
};
struct lt : public binop {
    static constexpr const int opcode = 10;
    static constexpr const char* name = "lt";
};

using instruction = std::variant <
    add, sub, mul, euc, lls, rls, myand, myor, eq, gt, lt
>;

std::ostream& operator<<(std::ostream& out, const instruction&);

instruction decode(std::uint16_t encoded);
std::uint16_t encode(instruction);

#endif

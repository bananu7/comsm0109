#include <instruction.hpp>
#include <register.hpp>
#include <type_traits>
#include <variant>
#include <iostream>
#include <string>

/* Instruction encoding 
For binary operations:
  bit: 0        4     7    10    13        16
field: | opcode | lhs | rhs | dst | [dst'] |

For binary lli:
  bit: 0        4     7    10    13        16
field: | opcode | lhs | rhs | dst | [dst'] |
*/

static const constexpr std::uint16_t opcode_mask = 0b111100000000000;
static const constexpr std::uint16_t op1_mask = 0b0000111000000000;
static const constexpr std::uint16_t op2_mask = op1_mask >> 3;
static const constexpr std::uint16_t op3_mask = op2_mask >> 3;
static const constexpr std::uint16_t op4_mask = op3_mask >> 3;


template<typename T>
static instruction decode_binop(std::int16_t encoded) {
    return T {
        static_cast<reg>(encoded && op1_mask),
        static_cast<reg>(encoded && op2_mask),
        static_cast<reg>(encoded && op3_mask)
    };
}

instruction decode(std::uint16_t encoded) {
    unsigned int opcode = encoded && opcode_mask;
    switch(opcode) {
    case add::opcode: return decode_binop<add>(encoded);
    case sub::opcode: return decode_binop<sub>(encoded);
    default: return {};
    }
}

template<typename T>
static std::uint16_t encode_binop(instruction poly_i) {
    const T& i = std::get<T>(poly_i);
    return T::opcode << 12
        | static_cast<std::uint16_t>(i.lhs) << 9
        | static_cast<std::uint16_t>(i.rhs) << 6
        | static_cast<std::uint16_t>(i.dst) << 3;
}

static std::uint16_t encode_euc(instruction poly_i) {
    const euc& i = std::get<euc>(poly_i);
    return euc::opcode << 12
        | static_cast<std::uint16_t>(i.lhs) << 9
        | static_cast<std::uint16_t>(i.rhs) << 6
        | static_cast<std::uint16_t>(i.dst_quo) << 3
        | static_cast<std::uint16_t>(i.dst_rem);
}

std::uint16_t encode(instruction instr) {
    return std::visit(
        [](auto i) -> std::uint16_t {
            if constexpr(std::is_base_of_v<binop, decltype(i)>) {
                return encode_binop<decltype(i)>(i);
            } else if constexpr(std::is_same_v<euc, decltype(i)>) {
                return encode_euc(i);
            }
            return 0;
        },
        instr
    );
}

std::ostream& operator<<(std::ostream& out, const instruction& poly_i) {
    out << "{";
    std::visit([&out](auto i) { out << decltype(i)::name; }, poly_i);
    std::visit([&out](auto i) {
                   if constexpr(std::is_base_of_v<binop, decltype(i)>) {
                      out << " " << i.lhs << " " << i.rhs << " " << i.dst; 
                   }
               }, poly_i);
    out << "}";
    return out;
}

#include <iostream>
#include <variant>
#include <cstdint>
#include <array>
#include <vector>
#include <stdexcept>
#include <optional>
#include <limits>
#include <register.hpp>
#include <instruction.hpp>

class cpu {
    int time;
    enum class state {
        idle, fetch, decode, execute
    } state;

    std::optional<instruction> decoded;
    std::array<std::int32_t, static_cast<std::size_t>(reg::g5)> register_file;
    std::array<std::int8_t, 1024 * 1024> main_memory; // 1MiB of memory

    std::int32_t& get(reg r) {
        return register_file[static_cast<int>(r)];
    }
    
    std::int8_t& get(std::size_t offset) {
        return main_memory.at(offset);
    }

    void step() {
        switch(state) {
        case state::idle: {
            break;
        }
        case state::fetch: {
            state = state::decode;
            break;
        }
        case state::decode: {
            decoded = decode(get(get(reg::pc)));
            state = state::execute;
            break;
        }
        case state::execute:
            std::cout << "Executing at " << get(reg::pc) << ": " << decoded.value() << "\n";
            std::visit( [&](auto i) {
                if constexpr(std::is_same_v<decltype(i), add>) {
                    std::cout << i.dst << " = " << get(i.lhs) << " + " << get(i.rhs) << "\n";
                    get(i.dst) = get(i.lhs) + get(i.rhs);
                }
            }, decoded.value());
            get(reg::pc)++;
            return;
        }
    }
    
public:
    cpu() : time{0}, state{state::idle}, register_file{0}, main_memory{0} {
        get(reg::pc) = 1;
    }

    // Clear the memory, Load the given binary, set pc = 1 and run. return the value in reg::g1 when pc=0
    std::int32_t operator()(const std::vector<std::uint8_t> binary) {
        if(binary.size() > main_memory.size()) {
            throw std::runtime_error("Binary too large!");
        }
        main_memory.fill(0);
        register_file.fill(0);
        get(reg::one) = 1;
        std::copy(binary.begin(), binary.end(), main_memory.begin());
        // start at 0x1
        get(reg::pc) = 2;
        state = state::fetch;
        while(get(reg::pc) != 0 && get(reg::pc) <= static_cast<int>(binary.size() / 2)) {
            step();
        }
        return get(reg::g1);
    }
};

int main() {
    //const instruction program = add { reg::one, reg::one, reg::g1 };
    std::vector<std::uint8_t> progbin(4);
    //*(reinterpret_cast<std::uint16_t*>(progbin.data()) + 1) = encode(program);
    *(reinterpret_cast<std::uint16_t*>(progbin.data()) + 1) = 0b0000001001011000;
    std::cout << decode(*(reinterpret_cast<std::uint16_t*>(progbin.data()) + 1)) << "\n---\n";

    cpu machine;
    int result = machine(progbin);
    std::cout << "---\n  program says 1 + 1 = " << result << "\n";
    return 0;
}

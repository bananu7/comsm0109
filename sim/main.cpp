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
            break;
        }
        case state::decode: {
            decoded = decode(get(get(reg::pc)));
            break;
        }
        case state::execute:
            return;
        }
    }
    
public:
    cpu() : time{0}, state{state::idle}, register_file{0}, main_memory{0} {
        get(reg::pc) = 1;
    }

    // Clear the memory, Load the given binary, set pc = 1 and run. return the value in reg::g1 when pc=0
    std::int32_t operator()(const std::vector<std::int8_t> binary) {
        if(binary.size() > main_memory.size()) {
            throw std::runtime_error("Binary too large!");
        }
        main_memory.fill(0);
        register_file.fill(0);
        std::copy(binary.begin(), binary.end(), main_memory.begin());
        get(reg::pc) == 1;
        while(get(reg::pc) != 0) {
            step();
        }
    }
};

int main() {
    instruction i = add { reg::g1, reg::g2, reg::g3 };
    std::cout << i << "\n";
    return 0;
}

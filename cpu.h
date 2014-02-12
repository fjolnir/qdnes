// Ricoh 2A03 emulator
#import <stdint.h>

typedef struct {
    uint8_t mem[0x10000]; // 64k
    union {
        struct {
            uint8_t acc;
            uint8_t x;
            uint8_t y;
            uint8_t sp; // Treated as a 16 bit integer with bit 9 set
            uint16_t pc;
            uint8_t flags;
        } regs;
        uint8_t regbuf[7];
    };
} taot; // TwoAOhThree

typedef enum {
    taot_carry_flag      = 1,
    taot_zero_flag       = 1<<1,
    taot_interrupt_flag  = 1<<2,
    taot_breakpoint_flag = 1<<4,
    taot_overflow_flag   = 1<<6,
    taot_sign_flag       = 1<<7
} taot_flag;

typedef enum {
    taot_immediate,    // INST #$0a
    taot_absolute,     // INST $ADDR
    taot_zeropage,     // INST $AD ;absolute on zero page
    taot_implied,      // INST
    taot_accum,        // INST ;data is on the accuulator
    taot_absolute_x,   // INST $OFSE, X ; *(x+$OFSE)
    taot_absolute_y,   // INST $OFSE, Y ; *(y+$OFSE)
    taot_zeropage_x,   // INST $OF, X
    taot_zeropage_y,   // INST $OF, X
    taot_indirect,     // INST ($ADDR)
    taot_indirect_x,   // INST ($AD, X) ; *(addr+x)
    taot_indirect_y    // INST ($AD), Y ; *addr + y
} taot_addr_mode;

typedef enum {
    taot_INVALID, // Invalid instrution

    taot_ADC,     // Add Memory to Accumulator with Carry
    taot_AND,     // "AND" Memory with Accumulator
    taot_ASL,     // Shift Left One Bit (Memory or Accumulator)

    taot_BCC,     // Branch on Carry Clear
    taot_BCS,     // Branch on Carry Set
    taot_BEQ,     // Branch on Result Zero
    taot_BIT,     // Test Bits in Memory with Accumulator
    taot_BMI,     // Branch on Result Minus
    taot_BNE,     // Branch on Result not Zero
    taot_BPL,     // Branch on Result Plus
    taot_BRK,     // Force Break
    taot_BVC,     // Branch on Overflow Clear
    taot_BVS,     // Branch on Overflow Set

    taot_CLC,     // Clear Carry Flag
    taot_CLD,     // Clear Decimal Mode
    taot_CLI,     // Clear interrupt Disable Bit
    taot_CLV,     // Clear Overflow Flag
    taot_CMP,     // Compare Memory and Accumulator
    taot_CPX,     // Compare Memory and Index X
    taot_CPY,     // Compare Memory and Index Y

    taot_DEC,     // Decrement Memory by One
    taot_DEX,     // Decrement Index X by One
    taot_DEY,     // Decrement Index Y by One

    taot_EOR,     // "Exclusive-Or" Memory with Accumulator

    taot_INC,     // Increment Memory by One
    taot_INX,     // Increment Index X by One
    taot_INY,     // Increment Index Y by One

    taot_JMP,     // Jump to New Location
    taot_JMPI,     // Jump to New Location

    taot_JSR,     // Jump to New Location Saving Return Address

    taot_LDA,     // Load Accumulator with Memory
    taot_LDX,     // Load Index X with Memory
    taot_LDY,     // Load Index Y with Memory
    taot_LSR,     // Shift Right One Bit (Memory or Accumulator)

    taot_NOP,     // No Operation

    taot_ORA,     // "OR" Memory with Accumulator

    taot_PHA,     // Push Accumulator on Stack
    taot_PHP,     // Push Processor Status on Stack
    taot_PLA,     // Pull Accumulator from Stack
    taot_PLP,     // Pull Processor Status from Stack

    taot_ROL,     // Rotate One Bit Left (Memory or Accumulator)
    taot_ROR,     // Rotate One Bit Right (Memory or Accumulator)
    taot_RTI,     // Return from Interrupt
    taot_RTS,     // Return from Subroutine

    taot_SBC,     // Subtract Memory from Accumulator with Borrow
    taot_SEC,     // Set Carry Flag
    taot_SED,     // Set Decimal Mode
    taot_SEI,     // Set Interrupt Disable Status
    taot_STA,     // Store Accumulator in Memory
    taot_STX,     // Store Index X in Memory
    taot_STY,     // Store Index Y in Memory

    taot_TAX,     // Transfer Accumulator to Index X
    taot_TAY,     // Transfer Accumulator to Index Y
    taot_TSX,     // Transfer Stack Pointer to Index X
    taot_TXA,     // Transfer Index X to Accumulator
    taot_TXS,     // Transfer Index X to Stack Pointer
    taot_TYA,     // Transfer Index Y to Accumulator

    taot_NOOP
} taot_inst;

static const uint64_t taot_translation_table[0xff] = {
    // Loads
    [0xa1] = taot_LDA | ((uint64_t)taot_indirect_x << 32),
    [0xa5] = taot_LDA | ((uint64_t)taot_zeropage   << 32),
    [0xa9] = taot_LDA | ((uint64_t)taot_immediate  << 32),
    [0xad] = taot_LDA | ((uint64_t)taot_absolute   << 32),
    [0xb1] = taot_LDA | ((uint64_t)taot_indirect_x << 32),
    [0xb5] = taot_LDA | ((uint64_t)taot_zeropage_x << 32),
    [0xb9] = taot_LDA | ((uint64_t)taot_absolute_x << 32),
    [0xbd] = taot_LDA | ((uint64_t)taot_absolute_y << 32),

    [0xa2] = taot_LDX | ((uint64_t)taot_immediate  << 32),
    [0xa6] = taot_LDX | ((uint64_t)taot_zeropage   << 32),
    [0xb6] = taot_LDX | ((uint64_t)taot_zeropage_y << 32),
    [0xae] = taot_LDX | ((uint64_t)taot_absolute   << 32),
    [0xbe] = taot_LDX | ((uint64_t)taot_absolute_y << 32),

    [0xa0] = taot_LDY | ((uint64_t)taot_immediate  << 32),
    [0xa4] = taot_LDY | ((uint64_t)taot_zeropage   << 32),
    [0xb4] = taot_LDY | ((uint64_t)taot_zeropage_y << 32),
    [0xac] = taot_LDY | ((uint64_t)taot_absolute   << 32),
    [0xbc] = taot_LDY | ((uint64_t)taot_absolute_y << 32),

    // Stores
    [0x85] = taot_STA | ((uint64_t)taot_zeropage_x << 32),
    [0x95] = taot_STA | ((uint64_t)taot_zeropage   << 32),
    [0x8d] = taot_STA | ((uint64_t)taot_absolute   << 32),
    [0x9d] = taot_STA | ((uint64_t)taot_absolute_x << 32),
    [0x99] = taot_STA | ((uint64_t)taot_absolute_y << 32),
    [0x81] = taot_STA | ((uint64_t)taot_indirect_x << 32),
    [0x91] = taot_STA | ((uint64_t)taot_indirect_y << 32),

    [0x86] = taot_STX | ((uint64_t)taot_zeropage   << 32),
    [0x96] = taot_STX | ((uint64_t)taot_zeropage_x << 32),
    [0x8e] = taot_STX | ((uint64_t)taot_absolute   << 32),

    [0x84] = taot_STY | ((uint64_t)taot_zeropage   << 32),
    [0x94] = taot_STY | ((uint64_t)taot_zeropage_x << 32),
    [0x8c] = taot_STY | ((uint64_t)taot_absolute   << 32),

    // Arithmetic
    [0x69] = taot_ADC | ((uint64_t)taot_immediate  << 32),
    [0x65] = taot_ADC | ((uint64_t)taot_zeropage   << 32),
    [0x75] = taot_ADC | ((uint64_t)taot_zeropage_x << 32),
    [0x6d] = taot_ADC | ((uint64_t)taot_absolute   << 32),
    [0x7d] = taot_ADC | ((uint64_t)taot_absolute_x << 32),
    [0x79] = taot_ADC | ((uint64_t)taot_absolute_y << 32),
    [0x61] = taot_ADC | ((uint64_t)taot_indirect_x << 32),
    [0x71] = taot_ADC | ((uint64_t)taot_indirect_y << 32),

    [0xe9] = taot_SBC | ((uint64_t)taot_immediate  << 32),
    [0xe5] = taot_SBC | ((uint64_t)taot_zeropage   << 32),
    [0xf5] = taot_SBC | ((uint64_t)taot_zeropage_x << 32),
    [0xed] = taot_SBC | ((uint64_t)taot_absolute   << 32),
    [0xfd] = taot_SBC | ((uint64_t)taot_absolute_x << 32),
    [0xf9] = taot_SBC | ((uint64_t)taot_absolute_y << 32),
    [0xe1] = taot_SBC | ((uint64_t)taot_indirect_x << 32),
    [0xf1] = taot_SBC | ((uint64_t)taot_indirect_y << 32),

    // Comparisons
    [0xc9] = taot_CMP | ((uint64_t)taot_immediate  << 32),
    [0xc5] = taot_CMP | ((uint64_t)taot_zeropage   << 32),
    [0xd5] = taot_CMP | ((uint64_t)taot_zeropage_x << 32),
    [0xcd] = taot_CMP | ((uint64_t)taot_absolute   << 32),
    [0xdd] = taot_CMP | ((uint64_t)taot_absolute_x << 32),
    [0xd9] = taot_CMP | ((uint64_t)taot_absolute_y << 32),
    [0xc1] = taot_CMP | ((uint64_t)taot_indirect_x << 32),
    [0xd1] = taot_CMP | ((uint64_t)taot_indirect_y << 32),

    [0xe0] = taot_CPX | ((uint64_t)taot_immediate  << 32),
    [0xe4] = taot_CPX | ((uint64_t)taot_zeropage_x << 32),
    [0xec] = taot_CPX | ((uint64_t)taot_absolute   << 32),

    [0xc0] = taot_CPY | ((uint64_t)taot_immediate  << 32),
    [0xc4] = taot_CPY | ((uint64_t)taot_zeropage_x << 32),
    [0xcc] = taot_CPY | ((uint64_t)taot_absolute   << 32),

    // Bitwise operations
    [0x29] = taot_AND | ((uint64_t)taot_immediate  << 32),
    [0x25] = taot_AND | ((uint64_t)taot_zeropage   << 32),
    [0x35] = taot_AND | ((uint64_t)taot_zeropage_x << 32),
    [0x2d] = taot_AND | ((uint64_t)taot_absolute   << 32),
    [0x3d] = taot_AND | ((uint64_t)taot_absolute_x << 32),
    [0x39] = taot_AND | ((uint64_t)taot_absolute_y << 32),
    [0x21] = taot_AND | ((uint64_t)taot_indirect_x << 32),
    [0x31] = taot_AND | ((uint64_t)taot_indirect_y << 32),

    [0x09] = taot_ORA | ((uint64_t)taot_immediate  << 32),
    [0x05] = taot_ORA | ((uint64_t)taot_zeropage   << 32),
    [0x15] = taot_ORA | ((uint64_t)taot_zeropage_x << 32),
    [0x0d] = taot_ORA | ((uint64_t)taot_absolute   << 32),
    [0x1d] = taot_ORA | ((uint64_t)taot_absolute_x << 32),
    [0x19] = taot_ORA | ((uint64_t)taot_absolute_y << 32),
    [0x01] = taot_ORA | ((uint64_t)taot_indirect_x << 32),
    [0x11] = taot_ORA | ((uint64_t)taot_indirect_y << 32),

    [0x49] = taot_EOR | ((uint64_t)taot_immediate  << 32),
    [0x45] = taot_EOR | ((uint64_t)taot_zeropage   << 32),
    [0x55] = taot_EOR | ((uint64_t)taot_zeropage_x << 32),
    [0x4d] = taot_EOR | ((uint64_t)taot_absolute   << 32),
    [0x5d] = taot_EOR | ((uint64_t)taot_absolute_x << 32),
    [0x59] = taot_EOR | ((uint64_t)taot_absolute_y << 32),
    [0x41] = taot_EOR | ((uint64_t)taot_indirect_x << 32),
    [0x51] = taot_EOR | ((uint64_t)taot_indirect_y << 32),

    [0x24] = taot_BIT | ((uint64_t)taot_zeropage   << 32),
    [0x2c] = taot_BIT | ((uint64_t)taot_absolute   << 32),

    // Shifts/Rotates
    [0x2a] = taot_ROL | ((uint64_t)taot_accum      << 32),
    [0x26] = taot_ROL | ((uint64_t)taot_zeropage   << 32),
    [0x36] = taot_ROL | ((uint64_t)taot_zeropage_x << 32),
    [0x2e] = taot_ROL | ((uint64_t)taot_absolute   << 32),
    [0x3e] = taot_ROL | ((uint64_t)taot_absolute_y << 32),

    [0x6a] = taot_ROR | ((uint64_t)taot_accum      << 32),
    [0x66] = taot_ROR | ((uint64_t)taot_zeropage   << 32),
    [0x76] = taot_ROR | ((uint64_t)taot_zeropage_x << 32),
    [0x6e] = taot_ROR | ((uint64_t)taot_absolute   << 32),
    [0x7e] = taot_ROR | ((uint64_t)taot_absolute_y << 32),

    [0x0a] = taot_ASL | ((uint64_t)taot_accum      << 32),
    [0x06] = taot_ASL | ((uint64_t)taot_zeropage   << 32),
    [0x16] = taot_ASL | ((uint64_t)taot_zeropage_x << 32),
    [0x0e] = taot_ASL | ((uint64_t)taot_absolute   << 32),
    [0x1e] = taot_ASL | ((uint64_t)taot_absolute_y << 32),

    [0x4a] = taot_LSR | ((uint64_t)taot_accum      << 32),
    [0x46] = taot_LSR | ((uint64_t)taot_zeropage   << 32),
    [0x56] = taot_LSR | ((uint64_t)taot_zeropage_x << 32),
    [0x4e] = taot_LSR | ((uint64_t)taot_absolute   << 32),
    [0x5e] = taot_LSR | ((uint64_t)taot_absolute_y << 32),

    // Incr/Decrements
    [0xe6] = taot_INC | ((uint64_t)taot_zeropage   << 32),
    [0xf6] = taot_INC | ((uint64_t)taot_zeropage_x << 32),
    [0xee] = taot_INC | ((uint64_t)taot_absolute   << 32),
    [0xfe] = taot_INC | ((uint64_t)taot_absolute_y << 32),

    [0xc6] = taot_DEC | ((uint64_t)taot_zeropage   << 32),
    [0xd6] = taot_DEC | ((uint64_t)taot_zeropage_x << 32),
    [0xce] = taot_DEC | ((uint64_t)taot_absolute   << 32),
    [0xde] = taot_DEC | ((uint64_t)taot_absolute_y << 32),

    [0xe8] = taot_INX,
    [0xca] = taot_DEX,
    [0xc8] = taot_INY,
    [0x88] = taot_DEY,

    // Register moves
    [0xaa] = taot_TAX,
    [0xa8] = taot_TAY,
    [0x8a] = taot_TXA,
    [0x98] = taot_TYA,
    [0x9a] = taot_TXS,
    [0xba] = taot_TSX,

    // Flag operations
    [0x18] = taot_CLC,
    [0x38] = taot_SEC,
    [0x58] = taot_CLI,
    [0x78] = taot_SEI,
    [0xb8] = taot_CLV,
    [0xd8] = taot_CLD,
    [0xf8] = taot_SED,

    // Branches
    [0x10] = taot_BPL,
    [0x30] = taot_BMI,
    [0x50] = taot_BVC,
    [0x70] = taot_BVS,
    [0x90] = taot_BCC,
    [0xb0] = taot_BCS,
    [0xd0] = taot_BNE,
    [0xf0] = taot_BEQ,

    // Jumps
    [0x4c] = taot_JMP,
    [0x6c] = taot_JMPI,

    // Procedure calls
    [0x20] = taot_JSR,
    [0x60] = taot_RTS,
    [0x00] = taot_BRK,
    [0x40] = taot_RTI,

    // Stack operations
    [0x48] = taot_PHA,
    [0x68] = taot_PLA,
    [0x08] = taot_PHP,
    [0x28] = taot_PLP,

    // No-op
    [0xea] = taot_NOOP,
};

static uint8_t taot_cycles[] = {
    /*0x00*/ 7,6,2,8,3,3,5,5,3,2,2,2,4,4,6,6,
    /*0x10*/ 2,5,2,8,4,4,6,6,2,4,2,7,4,4,7,7,
    /*0x20*/ 6,6,2,8,3,3,5,5,4,2,2,2,4,4,6,6,
    /*0x30*/ 2,5,2,8,4,4,6,6,2,4,2,7,4,4,7,7,
    /*0x40*/ 6,6,2,8,3,3,5,5,3,2,2,2,3,4,6,6,
    /*0x50*/ 2,5,2,8,4,4,6,6,2,4,2,7,4,4,7,7,
    /*0x60*/ 6,6,2,8,3,3,5,5,4,2,2,2,5,4,6,6,
    /*0x70*/ 2,5,2,8,4,4,6,6,2,4,2,7,4,4,7,7,
    /*0x80*/ 2,6,2,6,3,3,3,3,2,2,2,2,4,4,4,4,
    /*0x90*/ 2,6,2,6,4,4,4,4,2,5,2,5,5,5,5,5,
    /*0xA0*/ 2,6,2,6,3,3,3,3,2,2,2,2,4,4,4,4,
    /*0xB0*/ 2,5,2,5,4,4,4,4,2,4,2,4,4,4,4,4,
    /*0xC0*/ 2,6,2,8,3,3,5,5,2,2,2,2,4,4,6,6,
    /*0xD0*/ 2,5,2,8,4,4,6,6,2,4,2,7,4,4,7,7,
    /*0xE0*/ 2,6,3,8,3,3,5,5,2,2,2,2,4,4,6,6,
    /*0xF0*/ 2,5,2,8,4,4,6,6,2,4,2,7,4,4,7,7
};

void taot_init(taot *cpu);
void taot_cycle(taot *cpu);

void taot_dumpregs(taot *cpu);
void taot_dumpmem(taot *cpu, uint16_t start, uint16_t end);

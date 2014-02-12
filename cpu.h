// Ricoh 2A03 emulator (6502 with minor differences)
// http://www.masswerk.at/6502/6502_instruction_set.html
#import <stdint.h>
#import <stdbool.h>

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
    uint64_t cycles;
    bool crashed;
} taot; // TwoAOhThree

typedef enum {
    taot_interrupt,  // Standard Interrupt
    taot_nmi,        // Non-maskable
    taot_reset       // Reset CPU
} taot_irq_type;

static uint16_t const taot_irq_vectors[] = {
    [taot_nmi]       = 0xfffa,
    [taot_reset]     = 0xfffc,
    [taot_interrupt] = 0xfffe
};

typedef enum {
    taot_carry_flag       = 1,
    taot_zero_flag        = 1<<1,
    taot_int_disable_flag = 1<<2,
    taot_break_flag       = 1<<4,
    taot_overflow_flag    = 1<<6,
    taot_sign_flag        = 1<<7
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
    taot_indirect_y,   // INST ($AD), Y ; *addr + y
    taot_relative      // INST LABEL ;only used for branches
} taot_addr_mode;

#define TAOT_INSTRUCTIONS \
    TAOT_INST(INVALID) /* Invalid instrution */ \
\
    TAOT_INST(ADC)     /* Add Memory to Accumulator with Carry */ \
    TAOT_INST(AND)     /* "AND" Memory with Accumulator */ \
    TAOT_INST(ASL)     /* Shift Left One Bit (Memory or Accumulator) */ \
\
    TAOT_INST(BCC)     /* Branch on Carry Clear */ \
    TAOT_INST(BCS)     /* Branch on Carry Set */ \
    TAOT_INST(BEQ)     /* Branch on Result Zero */ \
    TAOT_INST(BIT)     /* Test Bits in Memory with Accumulator */ \
    TAOT_INST(BMI)     /* Branch on Result Minus */ \
    TAOT_INST(BNE)     /* Branch on Result not Zero */ \
    TAOT_INST(BPL)     /* Branch on Result Plus */ \
    TAOT_INST(BRK)     /* Force Break */ \
    TAOT_INST(BVC)     /* Branch on Overflow Clear */ \
    TAOT_INST(BVS)     /* Branch on Overflow Set */ \
\
    TAOT_INST(CLC)     /* Clear Carry Flag */ \
    TAOT_INST(CLD)     /* Clear Decimal Mode */ \
    TAOT_INST(CLI)     /* Clear interrupt Disable Bit */ \
    TAOT_INST(CLV)     /* Clear Overflow Flag */ \
    TAOT_INST(CMP)     /* Compare Memory and Accumulator */ \
    TAOT_INST(CPX)     /* Compare Memory and Index X */ \
    TAOT_INST(CPY)     /* Compare Memory and Index Y */ \
\
    TAOT_INST(DEC)     /* Decrement Memory by One */ \
    TAOT_INST(DEX)     /* Decrement Index X by One */ \
    TAOT_INST(DEY)     /* Decrement Index Y by One */ \
\
    TAOT_INST(EOR)     /* "Exclusive-Or" Memory with Accumulator */ \
\
    TAOT_INST(INC)     /* Increment Memory by One */ \
    TAOT_INST(INX)     /* Increment Index X by One */ \
    TAOT_INST(INY)     /* Increment Index Y by One */ \
\
    TAOT_INST(JMP)     /* Jump to New Location */ \
    TAOT_INST(JMPI)     /* Jump to New Location */ \
\
    TAOT_INST(JSR)     /* Jump to New Location Saving Return Address */ \
\
    TAOT_INST(LDA)     /* Load Accumulator with Memory */ \
    TAOT_INST(LDX)     /* Load Index X with Memory */ \
    TAOT_INST(LDY)     /* Load Index Y with Memory */ \
    TAOT_INST(LSR)     /* Shift Right One Bit (Memory or Accumulator) */ \
\
    TAOT_INST(NOP)     /* No Operation */ \
\
    TAOT_INST(ORA)     /* "OR" Memory with Accumulator */ \
\
    TAOT_INST(PHA)     /* Push Accumulator on Stack */ \
    TAOT_INST(PHP)     /* Push Processor Status on Stack */ \
    TAOT_INST(PLA)     /* Pull Accumulator from Stack */ \
    TAOT_INST(PLP)     /* Pull Processor Status from Stack */ \
\
    TAOT_INST(ROL)     /* Rotate One Bit Left (Memory or Accumulator) */ \
    TAOT_INST(ROR)     /* Rotate One Bit Right (Memory or Accumulator) */ \
    TAOT_INST(RTI)     /* Return from Interrupt */ \
    TAOT_INST(RTS)     /* Return from Subroutine */ \
\
    TAOT_INST(SBC)     /* Subtract Memory from Accumulator with Borrow */ \
    TAOT_INST(SEC)     /* Set Carry Flag */ \
    TAOT_INST(SED)     /* Set Decimal Mode */ \
    TAOT_INST(SEI)     /* Set Interrupt Disable Status */ \
    TAOT_INST(STA)     /* Store Accumulator in Memory */ \
    TAOT_INST(STX)     /* Store Index X in Memory */ \
    TAOT_INST(STY)     /* Store Index Y in Memory */ \
\
    TAOT_INST(TAX)     /* Transfer Accumulator to Index X */ \
    TAOT_INST(TAY)     /* Transfer Accumulator to Index Y */ \
    TAOT_INST(TSX)     /* Transfer Stack Pointer to Index X */ \
    TAOT_INST(TXA)     /* Transfer Index X to Accumulator */ \
    TAOT_INST(TXS)     /* Transfer Index X to Stack Pointer */ \
    TAOT_INST(TYA)     /* Transfer Index Y to Accumulator */ \
\
    TAOT_INST(NOOP)

typedef enum {
#define TAOT_INST(name) taot_##name,
    TAOT_INSTRUCTIONS
#undef TAOT_INST
} taot_inst;

static const char * taot_instruction_names[] = {
#define TAOT_INST(name) #name,
    TAOT_INSTRUCTIONS
#undef TAOT_INST
};
#undef TAOT_INSTRUCTIONS

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

    [0xe8] = taot_INX | ((uint64_t)taot_implied << 32),
    [0xca] = taot_DEX | ((uint64_t)taot_implied << 32),
    [0xc8] = taot_INY | ((uint64_t)taot_implied << 32),
    [0x88] = taot_DEY | ((uint64_t)taot_implied << 32),

    // Register moves
    [0xaa] = taot_TAX | ((uint64_t)taot_implied << 32),
    [0xa8] = taot_TAY | ((uint64_t)taot_implied << 32),
    [0x8a] = taot_TXA | ((uint64_t)taot_implied << 32),
    [0x98] = taot_TYA | ((uint64_t)taot_implied << 32),
    [0x9a] = taot_TXS | ((uint64_t)taot_implied << 32),
    [0xba] = taot_TSX | ((uint64_t)taot_implied << 32),

    // Flag operations
    [0x18] = taot_CLC | ((uint64_t)taot_implied << 32),
    [0x38] = taot_SEC | ((uint64_t)taot_implied << 32),
    [0x58] = taot_CLI | ((uint64_t)taot_implied << 32),
    [0x78] = taot_SEI | ((uint64_t)taot_implied << 32),
    [0xb8] = taot_CLV | ((uint64_t)taot_implied << 32),
    [0xd8] = taot_CLD | ((uint64_t)taot_implied << 32),
    [0xf8] = taot_SED | ((uint64_t)taot_implied << 32),

    // Branches
    [0x10] = taot_BPL | ((uint64_t)taot_relative << 32),
    [0x30] = taot_BMI | ((uint64_t)taot_relative << 32),
    [0x50] = taot_BVC | ((uint64_t)taot_relative << 32),
    [0x70] = taot_BVS | ((uint64_t)taot_relative << 32),
    [0x90] = taot_BCC | ((uint64_t)taot_relative << 32),
    [0xb0] = taot_BCS | ((uint64_t)taot_relative << 32),
    [0xd0] = taot_BNE | ((uint64_t)taot_relative << 32),
    [0xf0] = taot_BEQ | ((uint64_t)taot_relative << 32),

    // Jumps
    [0x4c] = taot_JMP | ((uint64_t)taot_implied << 32),
    [0x6c] = taot_JMPI | ((uint64_t)taot_implied << 32),

    // Procedure calls
    [0x20] = taot_JSR | ((uint64_t)taot_absolute << 32),
    [0x60] = taot_RTS | ((uint64_t)taot_absolute << 32),
    [0x00] = taot_BRK | ((uint64_t)taot_absolute << 32),
    [0x40] = taot_RTI | ((uint64_t)taot_absolute << 32),

    // Stack operations
    [0x48] = taot_PHA | ((uint64_t)taot_implied << 32),
    [0x68] = taot_PLA | ((uint64_t)taot_implied << 32),
    [0x08] = taot_PHP | ((uint64_t)taot_implied << 32),
    [0x28] = taot_PLP | ((uint64_t)taot_implied << 32),

    // No-op
    [0xea] = taot_NOOP | ((uint64_t)taot_implied << 32),
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

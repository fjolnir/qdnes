#import "cpu.h"
#import "utils.h"
#import <stdio.h>
#import <string.h>
#import <assert.h>

void taot_decode_opcode(uint8_t opcode, taot_inst *out_inst, taot_addr_mode *out_mode);
uint16_t taot_load_operand(taot *cpu, taot_addr_mode mode);
uint8_t taot_loadmem8(taot *cpu, uint16_t addr);
uint16_t taot_loadmem16(taot *cpu, uint16_t addr);
void taot_storemem8(taot *cpu, uint16_t addr, uint8_t val);
void taot_storemem16(taot *cpu, uint16_t addr, uint16_t val);
void taot_push8(taot *cpu, uint8_t val);
uint8_t taot_pop8(taot *cpu);
void taot_push16(taot *cpu, uint16_t val);
uint16_t taot_pop16(taot *cpu);
uint8_t taot_pop8_pc(taot *cpu);
uint16_t taot_pop16_pc(taot *cpu);

void taot_init(taot *cpu)
{
    // Reset memory
    memset(cpu->mem, 0, 0x10000);
    memset(cpu->mem, 0xffff, 0x2000);
//    for(int i = 0; i < 4*0x800; i += 0x800) {
//        cpu->mem[i + 0x008] = 0xF7;
//        cpu->mem[i + 0x009] = 0xEF;
//        cpu->mem[i + 0x00a] = 0xDF;
//        cpu->mem[i + 0x00f] = 0xBF;
//    }

    // Reset registers
    memset(cpu->regbuf, 0, sizeof(cpu->regbuf));
    cpu->regs.sp    = 0xff;
    cpu->regs.pc    = 0x8000-1;
    cpu->regs.flags = taot_int_disable_flag | taot_zero_flag;

}

void taot_cycle(taot *cpu)
{
    taot_inst op;
    taot_addr_mode addr_mode;
    uint16_t const old_pc = cpu->regs.pc;
    uint8_t const opcode = taot_pop8_pc(cpu);
    taot_decode_opcode(opcode, &op, &addr_mode);
    uint16_t const operand_addr = taot_load_operand(cpu, addr_mode);

    fprintf(stderr, "\e[34m0x%.4x\e[39m \e[1m%s\e[0m 0x%.4x\n", old_pc, taot_instruction_names[op], operand_addr);

#define SET_FLAG(name, status...) if(status) cpu->regs.flags |=  taot_##name##_flag; \
                                  else       cpu->regs.flags &= ~taot_##name##_flag;

#define SIGN_CHK(x)  SET_FLAG(sign, (x) & 0x80)
#define ZERO_CHK(x)  SET_FLAG(zero, (x) == 0)
#define CARRY_CHK(x) SET_FLAG(carry, (x) > 255)
#define BIT6_CHK(x)  SET_FLAG(overflow, ((x) >> 6)&1)

#define OVERFLOW_CHK(before, after) do { \
    typeof(before) const before_ = (before); \
    typeof(after)  const after_  = (after); \
    SET_FLAG(overflow, \
             ((before ^ cpu->regs.acc) & 0x80) == 0 \
          && ((after  ^ cpu->regs.acc) & 0x80) == 0x80); \
} while(0);

    uint32_t t1, t2; // For ops where the extra bits are helpful
    switch(op) {
        case taot_ADC: // add with carry
            t1 = cpu->regs.acc 
                 + (cpu->regs.flags|taot_carry_flag ? 1 : 0)
                 + taot_loadmem8(cpu, operand_addr);
            LETVAL(t1,
                   OVERFLOW_CHK(val, taot_loadmem8(cpu, operand_addr))
                   CARRY_CHK(val)
                   SIGN_CHK(val)
                   ZERO_CHK(val));
            cpu->regs.acc = t1 & 0xff;
            break;
        case taot_AND: // and (with accumulator)
            LETVAL(cpu->regs.acc &= taot_loadmem8(cpu, operand_addr),
                   SIGN_CHK(val)
                   ZERO_CHK(val));
            break;
        case taot_ASL: // arithmetic shift left
            if(addr_mode == taot_accum) {
                LETVAL(cpu->regs.acc,
                       CARRY_CHK(val));
                LETVAL(cpu->regs.acc <<= 1,
                       SIGN_CHK(val)
                       ZERO_CHK(val));
            } else {
                LETVAL(taot_loadmem8(cpu, operand_addr),
                       CARRY_CHK(val));
                taot_storemem8(cpu, operand_addr, taot_loadmem8(cpu, operand_addr) << 1);
                LETVAL(taot_loadmem8(cpu, operand_addr),
                       SIGN_CHK(val)
                       ZERO_CHK(val));
            }
            break;
        case taot_BCC: // branch on carry clear
            if((cpu->regs.flags & taot_carry_flag) == 0)
                cpu->regs.pc = operand_addr;
            break;
        case taot_BCS: // branch on carry set
            if((cpu->regs.flags & taot_carry_flag) != 0)
                cpu->regs.pc = operand_addr;
            break;
        case taot_BEQ: // branch on equal (zero set)
            if((cpu->regs.flags & taot_zero_flag) == 0)
                cpu->regs.pc = operand_addr;
            break;
        case taot_BIT: // bit test
            LETVAL((int32_t)taot_loadmem8(cpu, operand_addr),
                   SIGN_CHK(val)
                   BIT6_CHK(val)
                   ZERO_CHK(val & cpu->regs.acc));
            break;
        case taot_BMI: // branch on minus (negative set)
            if((cpu->regs.flags & taot_sign_flag) != 0)
                cpu->regs.pc = operand_addr;
            break;
        case taot_BNE: // branch on not equal (zero clear)
            if((cpu->regs.flags & taot_zero_flag) != 0)
                cpu->regs.pc = operand_addr;
            break;
        case taot_BPL: // branch on plus (negative clear)
            if((cpu->regs.flags & taot_sign_flag) == 0)
                cpu->regs.pc = operand_addr;
            break;
        case taot_BRK: // interrupt
            taot_push16(cpu, cpu->regs.pc);
            taot_push8(cpu, cpu->regs.flags);
            SET_FLAG(break, true);
            cpu->regs.pc = taot_loadmem16(cpu, taot_irq_vectors[taot_interrupt]);
            break;
        case taot_RTI: // return from interrupt
            cpu->regs.flags = taot_pop8(cpu);
            cpu->regs.pc    = taot_pop16(cpu);
            break;
        case taot_BVC: // branch on overflow clear
            if((cpu->regs.flags & taot_overflow_flag) == 0)
                cpu->regs.pc = operand_addr;
            break;
        case taot_BVS: // branch on overflow set
            if((cpu->regs.flags & taot_overflow_flag) == 1)
                cpu->regs.pc = operand_addr;
            break;
        case taot_CLC: // clear carry
            SET_FLAG(carry, false);
            break;
        case taot_CLD: // clear decimal
            goto unhandled_inst;
            break;
        case taot_CLI: // clear interrupt disable
            SET_FLAG(int_disable, false);
            break;
        case taot_CLV: // clear overflow
            SET_FLAG(overflow, false);
            break;
        case taot_CMP: // compare (with accumulator)
            LETVAL(cpu->regs.acc - taot_loadmem8(cpu, operand_addr),
                   SIGN_CHK(val)
                   SET_FLAG(carry, val >= 0)
                   SET_FLAG(zero, (val & 0xff) != 0));
            break;
        case taot_CPX: // compare with X
            LETVAL(cpu->regs.x - taot_loadmem8(cpu, operand_addr),
                   SIGN_CHK(val)
                   SET_FLAG(carry, val >= 0)
                   SET_FLAG(zero, (val & 0xff) != 0));
            break;
        case taot_CPY: // compare with Y
            LETVAL(cpu->regs.y - taot_loadmem8(cpu, operand_addr),
                   SIGN_CHK(val)
                   SET_FLAG(carry, val >= 0)
                   SET_FLAG(zero, (val & 0xff) != 0));
            break;
        case taot_DEC: // decrement
            taot_storemem8(cpu, operand_addr, taot_loadmem8(cpu, operand_addr)-1);
            LETVAL(taot_loadmem8(cpu, operand_addr),
                   SIGN_CHK(val)
                   ZERO_CHK(val));
            break;
        case taot_DEX: // decrement X
            LETVAL(--cpu->regs.x,
                   SIGN_CHK(val)
                   ZERO_CHK(val));
            break;
        case taot_DEY: // decrement Y
            LETVAL(--cpu->regs.x,
                   SIGN_CHK(val)
                   ZERO_CHK(val));
            break;
        case taot_EOR: // exclusive or (with accumulator)
            LETVAL(cpu->regs.acc ^= taot_loadmem8(cpu, operand_addr),
                   SIGN_CHK(val)
                   ZERO_CHK(val));
            break;
        case taot_INC: // increment
            taot_storemem8(cpu, operand_addr, taot_loadmem8(cpu, operand_addr)+1);
            LETVAL(taot_loadmem8(cpu, operand_addr),
                   SIGN_CHK(val)
                   ZERO_CHK(val));
            break;
        case taot_INX: // increment X
            LETVAL(++cpu->regs.x,
                   SIGN_CHK(val)
                   ZERO_CHK(val));
            break;
        case taot_INY: // increment Y
            LETVAL(++cpu->regs.x,
                   SIGN_CHK(val)
                   ZERO_CHK(val));
            break;
        case taot_JSR: // jump subroutine
            taot_push8(cpu, cpu->regs.pc >> 8);
            taot_push8(cpu, cpu->regs.pc & 0xff);
        case taot_JMP: // jump
            cpu->regs.pc = operand_addr;
            break;
        case taot_LDA: // load accumulator
            LETVAL(cpu->regs.acc = taot_loadmem8(cpu, operand_addr),
                   SIGN_CHK(val)
                   ZERO_CHK(val));
            break;
        case taot_LDX: // load X
            LETVAL(cpu->regs.x = taot_loadmem8(cpu, operand_addr),
                   SIGN_CHK(val)
                   ZERO_CHK(val));
            break;
        case taot_LDY: // load Y
            LETVAL(cpu->regs.y = taot_loadmem8(cpu, operand_addr),
                   SIGN_CHK(val)
                   ZERO_CHK(val));
            break;
        case taot_LSR: // logical shift right
            if(addr_mode == taot_accum) {
                LETVAL(cpu->regs.acc,
                       SET_FLAG(carry, val & 0x1));
                cpu->regs.acc >>= 1;
            } else {
                LETVAL(taot_loadmem8(cpu, operand_addr),
                       SET_FLAG(carry, val & 0x1));
                taot_storemem8(cpu, operand_addr, taot_loadmem8(cpu, operand_addr) >> 1);
            }
            break;
        case taot_NOP: // no operation
            break;
        case taot_ORA: // or with accumulator
            LETVAL(cpu->regs.acc |= taot_loadmem8(cpu, operand_addr),
                   SIGN_CHK(val)
                   ZERO_CHK(val));
            break;
        case taot_PHA: // push accumulator
            taot_push8(cpu, cpu->regs.acc);
            break;
        case taot_PHP: // push processor status (SR)
            taot_push8(cpu, cpu->regs.flags | taot_break_flag);
            break;
        case taot_PLA: // pull accumulator
            cpu->regs.acc = taot_pop8(cpu);
            break;
        case taot_PLP: // pull processor status (SR)
            cpu->regs.flags = taot_pop8(cpu);
            break;
        case taot_ROL: // rotate left
            t1 = addr_mode == taot_accum
                 ? cpu->regs.acc
                 : taot_loadmem8(cpu, operand_addr);
            t2 = cpu->regs.flags & taot_carry_flag;
            SET_FLAG(carry, (cpu->regs.acc >> 7) & 0x1);
            t1 = (cpu->regs.acc << 1) | t2;

            if(addr_mode == taot_accum)
                cpu->regs.acc = t1;
            else
                taot_storemem8(cpu, operand_addr, t1);
            break;
        case taot_ROR: // rotate right
            t1 = addr_mode == taot_accum
               ? cpu->regs.acc
               : taot_loadmem8(cpu, operand_addr);
            t2 = cpu->regs.flags & taot_carry_flag;
            SET_FLAG(carry, (cpu->regs.acc >> 7) & 0x1);
            t1 = (cpu->regs.acc >> 1) | (t2 << 7);

            if(addr_mode == taot_accum)
                cpu->regs.acc = t1;
            else
                taot_storemem8(cpu, operand_addr, t1);
            break;
        case taot_RTS: // return from subroutine
            cpu->regs.pc = taot_pop8(cpu) | (taot_pop8(cpu) << 8);
            break;
        case taot_SBC: // subtract with carry
            t1 = cpu->regs.acc 
               - (cpu->regs.flags|taot_carry_flag ? 0 : 1)
               - taot_loadmem8(cpu, operand_addr);
            LETVAL(t1,
                   OVERFLOW_CHK(val, taot_loadmem8(cpu, operand_addr))
                   CARRY_CHK(val)
                   SIGN_CHK(val)
                   ZERO_CHK(val));
            cpu->regs.acc = t1 & 0xff;
            break;
        case taot_SEC: // set carry
            SET_FLAG(carry, true);
            break;
        case taot_SED: // set decimal
            goto unhandled_inst;
            break;
        case taot_SEI: // set interrupt disable
            SET_FLAG(int_disable, true);
            break;
        case taot_STA: // store accumulator
            taot_storemem8(cpu, operand_addr, cpu->regs.acc);
            break;
        case taot_STX: // store X
            taot_storemem8(cpu, operand_addr, cpu->regs.x);
            break;
        case taot_STY: // store Y
            taot_storemem8(cpu, operand_addr, cpu->regs.y);
            break;
        case taot_TAX: // transfer accumulator to X
            LETVAL(cpu->regs.x = cpu->regs.acc,
                   SIGN_CHK(val)
                   ZERO_CHK(val));
            break;
        case taot_TAY: // transfer accumulator to Y
            LETVAL(cpu->regs.y = cpu->regs.acc,
                   SIGN_CHK(val)
                   ZERO_CHK(val));
            break;
        case taot_TSX: // transfer stack pointer to X
            LETVAL(cpu->regs.x = cpu->regs.sp,
                   SIGN_CHK(val)
                   ZERO_CHK(val));
            break;
        case taot_TXA: // transfer X to accumulator
            LETVAL(cpu->regs.acc = cpu->regs.x,
                   SIGN_CHK(val)
                   ZERO_CHK(val));
            break;
        case taot_TXS: // transfer X to stack pointer
            LETVAL(cpu->regs.sp = cpu->regs.x,
                   SIGN_CHK(val)
                   ZERO_CHK(val));
            break;
        case taot_TYA: // transfer Y to accumulator
            LETVAL(cpu->regs.acc = cpu->regs.y,
                   SIGN_CHK(val)
                   ZERO_CHK(val));
            break;
        case taot_INVALID:
        default:
            goto unhandled_inst;
    }

    cpu->cycles += taot_cycles[opcode];
    return;
unhandled_inst:
    fprintf(stderr, "unhandled instruction: 0x%x\n", (int)op);
    cpu->crashed = true;
}

uint8_t taot_loadmem8(taot *cpu, uint16_t addr)
{
    return cpu->mem[addr];
}
uint16_t taot_loadmem16(taot *cpu, uint16_t addr)
{
//    fprintf(stderr, "load16: 0x%x | (0x%x << 8)\n",  cpu->mem[addr], (cpu->mem[addr+1] << 8));
    return cpu->mem[addr] | (cpu->mem[addr+1] << 8);
}
void taot_storemem8(taot *cpu, uint16_t addr, uint8_t val)
{
     cpu->mem[addr] = val;
}
void taot_storemem16(taot *cpu, uint16_t addr, uint16_t val)
{
     cpu->mem[addr]   = val & 0xff;
     cpu->mem[addr+1] = val << 8;
}

void taot_decode_opcode(uint8_t opcode, taot_inst *out_inst, taot_addr_mode *out_mode)
{
    uint64_t const op = taot_translation_table[opcode];
    *out_inst = op & 0xffff;
    *out_mode = op >> 32;
}

uint16_t taot_load_operand(taot *cpu, taot_addr_mode mode)
{
    switch(mode) {
        case taot_immediate:
            return cpu->regs.pc++;
        case taot_implied:
            return 0;
        case taot_absolute:
            return taot_pop16_pc(cpu);
        case taot_zeropage:
            return taot_pop8_pc(cpu);
        case taot_accum:
            return cpu->regs.acc;
        case taot_absolute_x:
            return taot_pop16_pc(cpu) + cpu->regs.x;
        case taot_absolute_y:
            return taot_pop16_pc(cpu) + cpu->regs.y;
        case taot_zeropage_x:
            return (taot_pop8_pc(cpu) + cpu->regs.x) & 0xff;
        case taot_zeropage_y:
            return (taot_pop8_pc(cpu) + cpu->regs.y) & 0xff;
        case taot_indirect:
            LETVAL(taot_pop16_pc(cpu),
                return taot_loadmem8(cpu, val)
                     | ((uint16_t)taot_loadmem8(cpu, val+1) << 8);
            );
        case taot_indirect_x:
            return taot_loadmem16(cpu, (taot_pop8_pc(cpu) + cpu->regs.x) & 0xff);
        case taot_indirect_y:
            return taot_loadmem16(cpu, (taot_pop8_pc(cpu) + cpu->regs.y));
        case taot_relative:
            return cpu->regs.pc + 1 + (int8_t)taot_pop8_pc(cpu);
        default:
            fprintf(stderr, "Invalid addressing mode: %d\n", mode);
            cpu->crashed = true;
    }
}

void taot_push8(taot *cpu, uint8_t val)
{
    taot_storemem8(cpu, 0x100 | cpu->regs.sp, val);
    --cpu->regs.sp;
}
uint8_t taot_pop8(taot *cpu)
{
    return taot_loadmem8(cpu, cpu->regs.sp++);
}

void taot_push16(taot *cpu, uint16_t val)
{
    taot_storemem16(cpu, cpu->regs.sp, val);
    cpu->regs.sp -= 2;
}
uint16_t taot_pop16(taot *cpu)
{
    uint16_t const val = taot_loadmem16(cpu, cpu->regs.sp);
    cpu->regs.sp += 2;
    return val;
}
uint8_t taot_pop8_pc(taot *cpu)
{
    return taot_loadmem8(cpu, cpu->regs.pc++);
}
uint16_t taot_pop16_pc(taot *cpu)
{
    uint16_t const val = taot_loadmem16(cpu, cpu->regs.pc);
    cpu->regs.pc += 2;
    return val;
}

void taot_irq(taot *cpu, taot_irq_type type)
{
    if(type == taot_interrupt && (cpu->regs.flags & taot_int_disable_flag))
        return;

    taot_push16(cpu, cpu->regs.pc);
    taot_push8(cpu, cpu->regs.flags);

    cpu->regs.pc = taot_loadmem16(cpu, taot_irq_vectors[type]);
}

void taot_dumpregs(taot *cpu)
{
    fprintf(stderr, "\e[1mACC\e[0m    = 0x%x\n", cpu->regs.acc);
    fprintf(stderr, "\e[1mX\e[0m      = 0x%x\n", cpu->regs.x);
    fprintf(stderr, "\e[1mY\e[0m      = 0x%x\n", cpu->regs.y);
    fprintf(stderr, "\e[1mSP\e[0m     = 0x%x\n", cpu->regs.sp);
    fprintf(stderr, "\e[1mPC\e[0m     = 0x%x\n", cpu->regs.pc);
    fprintf(stderr, "\e[1mSTATUS\e[0m = 0x%x\n\n", cpu->regs.flags);
}

void taot_dumpmem(taot *cpu, uint16_t start, uint16_t end)
{
    uint16_t const line_len = 16;
    start -= MIN(start, (start + 1)%line_len);
    if(end == 0)
        end = sizeof(cpu->mem)-1;
    else
        end = MIN(sizeof(cpu->mem)-1, end + (end + 1)%line_len);

    for(uint16_t i = start; i <= end; i += line_len) {
        fprintf(stderr, "\e[34m0x%.4x\e[39m \e[31m", i);
        // HEX
        for(uint16_t j = i; j < i+line_len; j += 4) {
            fprintf(stderr, "%.2x%.2x%.2x%.2x ",
                    cpu->mem[j], cpu->mem[j+1], cpu->mem[j+2],cpu->mem[j+3]);
        }
        // ASCII
        char str_buf[line_len];
        memcpy(str_buf, cpu->mem+i, sizeof(str_buf));
        for(int i = 0; i < sizeof(str_buf); ++i)
            if(!INRANGE(str_buf[i], 0x20, 0x7e)) str_buf[i] = '.';
        fprintf(stderr, "\e[39m| %s\n", str_buf);
    }
}

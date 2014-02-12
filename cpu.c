#import "cpu.h"
#import "utils.h"
#import <stdio.h>
#import <string.h>
#import <assert.h>

uint64_t taot_loadinst(taot *cpu, taot_inst *out_inst, taot_addr_mode *out_mode);
uint16_t taot_load_operand(taot *cpu, uint32_t pc, taot_addr_mode mode);
uint8_t taot_loadmem8(taot *cpu, uint16_t addr);
uint16_t taot_loadmem16(taot *cpu, uint16_t addr);
void taot_storemem8(taot *cpu, uint16_t addr, uint8_t val);

void taot_init(taot *cpu)
{
    // Reset memory
    memset(cpu->mem, 0, sizeof(cpu->mem));
    memset(cpu->mem, 0xffff, 0x1000 / 2);
    for(int i = 0; i < 4*0x800; i += 0x800) {
        cpu->mem[i + 0x008] = 0xF7;
        cpu->mem[i + 0x009] = 0xEF;
        cpu->mem[i + 0x00a] = 0xDF;
        cpu->mem[i + 0x00f] = 0xBF;
    }

    // Reset registers
    memset(cpu->regbuf, 0, sizeof(cpu->regbuf));
    cpu->regs.sp    = 0xff;
    cpu->regs.pc    = 0x8000-1;
    cpu->regs.flags = taot_interrupt_flag | taot_zero_flag;

}

void taot_cycle(taot *cpu)
{
    taot_inst op;
    taot_addr_mode addr_mode;
    uint32_t const op_addr = taot_loadinst(cpu, &op, &addr_mode);
    uint16_t const operand_addr = taot_load_operand(cpu, op_addr, addr_mode);
    cpu->regs.pc += taot_operand_sizes[addr_mode];

    fprintf(stderr, "op@0x%x: 0x%x operand: 0x%x\n", op_addr, op, addr_mode);
//    taot_perform_op(cpu, op, operand_addr);

#define SIGN_CHK(x)     (((x) & 0x80)   ? taot_sign_flag     : 0)
#define ZERO_CHK(x)     (((x) == 0)     ? taot_zero_flag     : 0)
#define CARRY_CHK(x)    (((x) > 255)    ? taot_carry_flag    : 0)
#define BIT6_CHK(x)     ((((x) >> 6)&1) ? taot_overflow_flag : 0)
#define OVERFLOW_CHK(x, res) ( \
        (   (((x)   ^ cpu->regs.acc) & 0x80) == 0 \
         && (((res) ^ cpu->regs.acc) & 0x80) == 0x80 \
        ) \
        ? taot_overflow_flag : 0 \
)
#define UPDATE_FLAGS(new_flags...) cpu->regs.flags &= ~(new_flags)
    uint32_t temp; // For ops where the extra bits are helpful
    switch(op) {
        case taot_ADC: // add with carry
            temp = cpu->regs.acc 
                 + (cpu->regs.flags|taot_carry_flag ? 1 : 0)
                 + taot_loadmem8(cpu, operand_addr);
            UPDATE_FLAGS(OVERFLOW_CHK(taot_loadmem8(cpu, operand_addr), temp)
                       | CARRY_CHK(temp)
                       | SIGN_CHK(temp)
                       | ZERO_CHK(temp));
            cpu->regs.acc = temp & 0xff;
            break;
        case taot_AND: // and (with accumulator)
            cpu->regs.acc &= taot_loadmem8(cpu, operand_addr);
            UPDATE_FLAGS(SIGN_CHK(cpu->regs.acc) | ZERO_CHK(cpu->regs.acc));
            break;
        case taot_ASL: // arithmetic shift left
            if(addr_mode == taot_accum) {
                UPDATE_FLAGS(CARRY_CHK(cpu->regs.acc));
                cpu->regs.acc >>= 1;
                UPDATE_FLAGS(SIGN_CHK(cpu->regs.acc) | ZERO_CHK(cpu->regs.acc));
            } else {
                UPDATE_FLAGS(CARRY_CHK(taot_loadmem8(cpu, operand_addr)));
                taot_storemem8(cpu, operand_addr, taot_loadmem8(cpu, operand_addr) << 1);
                UPDATE_FLAGS(SIGN_CHK(taot_loadmem8(cpu, operand_addr))
                           | ZERO_CHK(taot_loadmem8(cpu, operand_addr)));
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
            temp = taot_loadmem8(cpu, operand_addr);
            UPDATE_FLAGS(SIGN_CHK(temp)
                       | BIT6_CHK(temp)
                       | ZERO_CHK(temp & cpu->regs.acc));
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
            goto unhandled_inst; // TODO
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
            cpu->regs.flags &= ~(taot_carry_flag);
            break;
        case taot_CLD: // clear decimal
            goto unhandled_inst;
            break;
        case taot_CLI: // clear interrupt disable
            cpu->regs.flags &= ~(taot_interrupt_flag);
            break;
        case taot_CLV: // clear overflow
            cpu->regs.flags &= ~(taot_overflow_flag);
            break;
        case taot_CMP: // compare (with accumulator)
            temp = cpu->regs.acc - taot_loadmem8(cpu, operand_addr);
            UPDATE_FLAGS(SIGN_CHK(temp)
                       | (temp >= 0) ? taot_carry_flag : 0
                       | ((temp & 0xff) != 0) ? taot_zero_flag : 0);
            break;
        case taot_CPX: // compare with X
            temp = cpu->regs.x - taot_loadmem8(cpu, operand_addr);
            UPDATE_FLAGS(SIGN_CHK(temp)
                       | (temp >= 0) ? taot_carry_flag : 0
                       | ((temp & 0xff) != 0) ? taot_zero_flag : 0);
            break;
        case taot_CPY: // compare with Y
            temp = cpu->regs.y - taot_loadmem8(cpu, operand_addr);
            UPDATE_FLAGS(SIGN_CHK(temp)
                       | (temp >= 0) ? taot_carry_flag : 0
                       | ((temp & 0xff) != 0) ? taot_zero_flag : 0);
            break;
        case taot_DEC: // decrement
            taot_storemem8(cpu, operand_addr, taot_loadmem8(cpu, operand_addr)-1);
            UPDATE_FLAGS(SIGN_CHK(taot_loadmem8(cpu, operand_addr))
                       | ZERO_CHK(taot_loadmem8(cpu, operand_addr)));
            break;
        case taot_DEX: // decrement X
            --cpu->regs.x;
            UPDATE_FLAGS(SIGN_CHK(cpu->regs.x)
                       | ZERO_CHK(cpu->regs.x));
            break;
        case taot_DEY: // decrement Y
            --cpu->regs.y;
            UPDATE_FLAGS(SIGN_CHK(cpu->regs.y)
                       | ZERO_CHK(cpu->regs.y));
            break;
        case taot_EOR: // exclusive or (with accumulator)
            break;
        case taot_INC: // increment
            break;
        case taot_INX: // increment X
            break;
        case taot_INY: // increment Y
            break;
        case taot_JMP: // jump
            break;
        case taot_JSR: // jump subroutine
            break;
        case taot_LDA: // load accumulator
            break;
        case taot_LDX: // load X
            cpu->regs.x = taot_loadmem8(cpu, operand_addr);
            UPDATE_FLAGS(SIGN_CHK(cpu->regs.x) | ZERO_CHK(cpu->regs.x));
            break;
        case taot_LDY: // load Y
            cpu->regs.y = taot_loadmem8(cpu, operand_addr);
            UPDATE_FLAGS(SIGN_CHK(cpu->regs.y) | ZERO_CHK(cpu->regs.y));
            break;
        case taot_LSR: // logical shift right
            break;
        case taot_NOP: // no operation
            break;
        case taot_ORA: // or with accumulator
            break;
        case taot_PHA: // push accumulator
            break;
        case taot_PHP: // push processor status (SR)
            break;
        case taot_PLA: // pull accumulator
            break;
        case taot_PLP: // pull processor status (SR)
            break;
        case taot_ROL: // rotate left
            break;
        case taot_ROR: // rotate right
            break;
        case taot_RTI: // return from interrupt
            break;
        case taot_RTS: // return from subroutine
            break;
        case taot_SBC: // subtract with carry
            break;
        case taot_SEC: // set carry
            break;
        case taot_SED: // set decimal
            break;
        case taot_SEI: // set interrupt disable
            break;
        case taot_STA: // store accumulator
            break;
        case taot_STX: // store X
            taot_storemem8(cpu, operand_addr, cpu->regs.x);
            break;
        case taot_STY: // store Y
            taot_storemem8(cpu, operand_addr, cpu->regs.y);
            break;
        case taot_TAX: // transfer accumulator to X
            break;
        case taot_TAY: // transfer accumulator to Y
            break;
        case taot_TSX: // transfer stack pointer to X
            break;
        case taot_TXA: // transfer X to accumulator
            break;
        case taot_TXS: // transfer X to stack pointer
            break;
        case taot_TYA: // transfer Y to accumulator
            break;
        case taot_INVALID:
        default:
            goto unhandled_inst;
    }
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

uint64_t taot_loadinst(taot *cpu, taot_inst *out_inst, taot_addr_mode *out_mode)
{
    uint64_t const op = taot_translation_table[taot_loadmem8(cpu, cpu->regs.pc)];

    *out_inst = op & 0xffff;
    *out_mode = op >> 32;

    return cpu->regs.pc++;
}

uint16_t taot_load_operand(taot *cpu, uint32_t op_addr, taot_addr_mode mode)
{
    switch(mode) {
        case taot_immediate:
            return op_addr+1;
        case taot_implied:
            return 0;
        case taot_absolute:
            return taot_loadmem16(cpu, op_addr+1);
        case taot_zeropage:
            return taot_loadmem8(cpu, op_addr+1);
        case taot_accum:
            return cpu->regs.acc;
        case taot_absolute_x:
            return taot_loadmem16(cpu, op_addr+1) + cpu->regs.x;
        case taot_absolute_y:
            return taot_loadmem16(cpu, op_addr+1) + cpu->regs.y;
        case taot_zeropage_x:
            return (taot_loadmem8(cpu, op_addr+1) + cpu->regs.x) & 0xff;
        case taot_zeropage_y:
            return (taot_loadmem8(cpu, op_addr+1) + cpu->regs.y) & 0xff;
        case taot_indirect:
            assert(0); // TODO!!!
//            uint16_t const addr = taot_loadmem16(cpu, op_addr+2);
        case taot_indirect_x:
            return taot_loadmem16(cpu, (taot_loadmem8(cpu, op_addr+2) + cpu->regs.x) & 0xff);
        case taot_indirect_y:
            return taot_loadmem16(cpu, (taot_loadmem8(cpu, op_addr+2)) + cpu->regs.y);
        default:
            fprintf(stderr, "Invalid addressing mode: %d\n", mode);
            assert(0);
    }
}


void taot_dumpregs(taot *cpu)
{
    fputs("Registers:\n", stderr);
    fprintf(stderr, "  ACC    = 0x%x\n", cpu->regs.acc);
    fprintf(stderr, "  X      = 0x%x\n", cpu->regs.x);
    fprintf(stderr, "  Y      = 0x%x\n", cpu->regs.y);
    fprintf(stderr, "  SP     = 0x%x\n", cpu->regs.sp);
    fprintf(stderr, "  PC     = 0x%x\n", cpu->regs.pc);
    fprintf(stderr, "  STATUS = 0x%x\n\n", cpu->regs.flags);
}

void taot_dumpmem(taot *cpu, uint16_t start, uint16_t end)
{
    uint16_t const line_len = 16;
    start -= MIN(start, (start + 1)%line_len);
    if(end == 0)
        end = sizeof(cpu->mem)-1;
    else
        end = MIN(sizeof(cpu->mem)-1, end + (end + 1)%line_len);

    fprintf(stderr, "Bytes %d to %d (out of: %lu):\n", start, end, sizeof(cpu->mem));
    for(uint16_t i = start; i <= end; i += line_len) {
        fprintf(stderr, "0x%.4x: ", i);
        // HEX
        for(uint16_t j = i; j < i+line_len; j += 4) {
            fprintf(stderr, "%.2x%.2x%.2x%.2x ",
                    cpu->mem[j], cpu->mem[j+1], cpu->mem[j+2],cpu->mem[j+3]);
        }
        // ASCII
        char str_buf[line_len];
        char *c = str_buf;
        memcpy(str_buf, cpu->mem+i, sizeof(str_buf));
        DUFF(sizeof(str_buf), if(!INRANGE(*c++, 0x20, 0x7e)) *(c-1) = '.');
        fprintf(stderr, "| %s\n", str_buf);
    }
}

#import "cpu.h"
#import "utils.h"
#import <stdio.h>
#import <string.h>
#import <assert.h>

uint64_t taot_loadinst(taot *cpu, taot_inst *out_inst, taot_addr_mode *out_mode);
uint16_t taot_load_operand(taot *cpu, uint32_t pc, taot_addr_mode mode);
uint8_t taot_loadmem8(taot *cpu, int32_t addr);
uint16_t taot_loadmem16(taot *cpu, int32_t addr);

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

//    taot_perform_op(cpu, op, operand_addr);

    switch(op) {
        case taot_LDX:
            cpu->regs.x = taot_loadmem8(cpu, operand_addr);
            cpu->regs.flags &= ~(
                  ((cpu->regs.x&0x80) ? taot_sign_flag : 0)
                | ((cpu->regs.x == 0) ? taot_zero_flag : 0)
            );
            break;
        default:
            fprintf(stderr, "unhandled instruction: 0x%x\n", (int)op);
            assert(0);
    }
}

uint8_t taot_loadmem8(taot *cpu, int32_t addr)
{
    return cpu->mem[addr];
}
uint16_t taot_loadmem16(taot *cpu, int32_t addr)
{
    return cpu->mem[addr] | (cpu->mem[addr+1] << 8);
}

uint64_t taot_loadinst(taot *cpu, taot_inst *out_inst, taot_addr_mode *out_mode)
{
    uint64_t const op = taot_translation_table[taot_loadmem8(cpu, cpu->regs.pc)];

    *out_inst = op & 0xffff;
    *out_mode = op >> 32;
    assert(*out_inst != taot_INVALID);

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
            return taot_loadmem16(cpu, op_addr+2);
        case taot_zeropage:
            return taot_loadmem8(cpu, op_addr+2);
        case taot_accum:
            return cpu->regs.acc;
        case taot_absolute_x:
            return taot_loadmem16(cpu, op_addr+2) + cpu->regs.x;
        case taot_absolute_y:
            return taot_loadmem16(cpu, op_addr+2) + cpu->regs.y;
        case taot_zeropage_x:
            return (taot_loadmem8(cpu, op_addr+2) + cpu->regs.x) & 0xff;
        case taot_zeropage_y:
            return (taot_loadmem8(cpu, op_addr+2) + cpu->regs.y) & 0xff;
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
    start -= MIN(start, (start + 1)%16);
    if(end == 0)
        end = sizeof(cpu->mem)-1;
    else
        end = MIN(sizeof(cpu->mem)-1, end + (end + 1)%16);

    fprintf(stderr, "Bytes %d to %d (out of: %lu):\n", start, end, sizeof(cpu->mem));
    for(uint16_t i = start; i <= end; i += 16) {
        fprintf(stderr, "%6.1d: ", i);
        // HEX
        for(uint16_t j = i; j < i+16; j += 4) {
            fprintf(stderr, "%.2x%.2x%.2x%.2x ",
                    cpu->mem[j], cpu->mem[j+1], cpu->mem[j+2],cpu->mem[j+3]);
        }
        // ASCII
        char str_buf[16];
        char *c = str_buf;
        memcpy(str_buf, cpu->mem+i, sizeof(str_buf));
        DUFF(sizeof(str_buf), if(!INRANGE(*c++, 0x20, 0x7e)) *(c-1) = '.');
        fprintf(stderr, "| %s", str_buf);
        fputs("\n", stderr);
    }
}

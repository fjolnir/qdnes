#import "cpu.h"
#import <stdio.h>
#import <string.h>

int main(int argc, char *argv[])
{
    taot cpu;
    taot_init(&cpu);

    taot_dumpregs(&cpu);

    // Load a simple program (http://www.masswerk.at/6502/assembler.html)
    uint8_t loop5x[] = {
        0xA2, 0x00, 0xA9, 0x05, 0xE8, 0x8E,
        0x04, 0x00, 0xCD, 0x04, 0x00, 0xD0,
        0xF7
    };
    uint8_t fibonacci[] = {
        0xa0, 0x08, 0xa9, 0x00, 0x8d, 0x34,
        0x12, 0xa9, 0x01, 0xaa, 0x18, 0x6d,
        0x34, 0x12, 0x8e, 0x34, 0x12, 0x88,
        0xd0, 0xf5, 0xea
    };
#define program fibonacci
    memcpy(cpu.mem + cpu.regs.pc, program, sizeof(program));

    taot_dumpmem(&cpu, cpu.regs.pc, cpu.regs.pc + 31);
    fputs("\n---------------\n\n", stderr);
    while(!cpu.crashed) {
        taot_cycle(&cpu);
    }

    fputs("\n---------------\n\n", stderr);
    taot_dumpregs(&cpu);
    taot_dumpmem(&cpu, 0, 0xff);

    fprintf(stderr, "\nRan for %llu cycles (PC=0x%x)\n", cpu.cycles, cpu.regs.pc);
    return 0;
}

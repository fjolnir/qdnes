#import "cpu.h"
#import <stdio.h>
#import <string.h>

int main(int argc, char *argv[])
{
    printf("QDNES!\n");

    taot cpu;
    taot_init(&cpu);

    taot_dumpregs(&cpu);
    fputs("Running program:\n", stderr);

    // Load a simple program (http://www.masswerk.at/6502/assembler.html)
    uint8_t program[] = {
        0xa2, 0x1,     // LDX #1
        0xac, 0x8, 0x0, // LDY $0
        0x8e, 0x0, 0x0, // STX $0
    };
    memcpy(cpu.mem + cpu.regs.pc, program, sizeof(program));

    taot_dumpmem(&cpu, cpu.regs.pc, cpu.regs.pc + 31);
    fputs("\n---------------\n\n", stderr);
    while(!cpu.crashed) {
        taot_cycle(&cpu);
    }

    fputs("\n---- Result ----\n\n", stderr);
    taot_dumpregs(&cpu);
    taot_dumpmem(&cpu, 0, 0xff);
    return 0;
}

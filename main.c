#import "cpu.h"
#import <stdio.h>
#import <string.h>

int main(int argc, char *argv[])
{
    taot cpu;
    taot_init(&cpu);

    taot_dumpregs(&cpu);

    // Load a simple program (http://www.masswerk.at/6502/assembler.html)
    uint8_t program[] = {
        0xA2, 0x00, 0xA9, 0x05, 0xE8, 0x8E,
        0x00, 0x00, 0xCD, 0x00, 0x00, 0xD0,
        0xF7
    };
    memcpy(cpu.mem + cpu.regs.pc, program, sizeof(program));

    taot_dumpmem(&cpu, cpu.regs.pc, cpu.regs.pc + 31);
    fputs("\n---------------\n\n", stderr);
    while(!cpu.crashed) {
        taot_cycle(&cpu);
    }

    fputs("\n---------------\n\n", stderr);
    taot_dumpregs(&cpu);
    taot_dumpmem(&cpu, 0, 0xff);
    return 0;
}

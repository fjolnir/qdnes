#import "cpu.h"
#import <stdio.h>
#import <string.h>

int main(int argc, char *argv[])
{
    printf("QDNES!\n");

    taot cpu;
    taot_init(&cpu);

    taot_dumpregs(&cpu);
    fputs("Running program...\n", stderr);

    // Load a simple program
    uint8_t program[] = {
        0xa2, 0x0c
    };
    memcpy(cpu.mem + cpu.regs.pc, program, sizeof(program));

    taot_cycle(&cpu);

    fputs("\n---- Result ----\n\n", stderr);
    taot_dumpregs(&cpu);
    taot_dumpmem(&cpu, 0, 0xff);
    return 0;
}

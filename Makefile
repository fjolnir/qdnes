all:
	@clang -O0 -g cpu.c main.c -o qdnes
	@./qdnes


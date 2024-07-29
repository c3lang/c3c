SRCS_C3 := $(wildcard *.c3)

default: hello.elf

hello.a: $(SRCS_C3)
	c3c --use-stdlib=no --no-entry --target elf-riscv32 static-lib $(SRCS_C3)

start.o: start.s
	riscv64-unknown-elf-as -g -march=rv32i -mabi=ilp32 -o start.o start.s

hello.elf: hello.a start.o baremetal.ld
	riscv64-unknown-elf-ld -T baremetal.ld -m elf32lriscv -o hello.elf hello.a start.o

run: hello.elf
	@echo "Ctrl-A C for QEMU console, then quit to exit"
	qemu-system-riscv32 -nographic -serial mon:stdio -machine virt -bios hello.elf

debug: hello.elf
	@echo "Ctrl-A C for QEMU console, then quit to exit"
	qemu-system-riscv32 -nographic -serial mon:stdio -machine virt -s -S -bios hello.elf

clean:
	rm -f *.o *.a hello.elf

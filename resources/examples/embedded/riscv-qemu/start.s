# Simple C runtime startup bootstrap
# Two primary functions:
# - Stack allocation and initializing stack pointer
# - Jumping to main

.section .text._start
.global _start
_start:
  la sp, __stack_top      # Load the stack pointer
  add s0, sp, zero        # Set the frame pointer
  jal zero, main          # Run main entry point - no argc
loop:	j loop              # Spin forever in case main returns

# Support semi-hosting calls
.option norvc
.text
.balign 16
.global sys_semihost
.type sys_semihost @function
sys_semihost:             # https://github.com/riscv-non-isa/riscv-semihosting/blob/main/src/binary-interface.adoc
  slli zero, zero, 0x1f
  ebreak
  srai zero, zero, 0x7
  ret

.section .data
.space 1024*8             # allocate 8K of memory to serve as initial stack
.align 16                 # Smallest stack allocation is 16 bytes, so align accordingly
__stack_top:              # The stack grows downward according the Risc-V ABI

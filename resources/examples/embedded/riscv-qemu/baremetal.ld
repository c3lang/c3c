EXTERN(main)

SECTIONS
{
  . = 0x80000000;													/* QEMU default load address to run bios */
  .text : {
		KEEP(*(.text._start));								/* Ensure _start is placed first */
		*(.text*);														/* Program code here */
	}
	. = ALIGN (CONSTANT (COMMONPAGESIZE));	/* Make sure linker does not jam data into text section, making text writable */
  .data : { 
		*(.data*)															/* Stack goes here */
	}
}
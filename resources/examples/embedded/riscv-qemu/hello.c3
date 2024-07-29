import uart;

const UART0_BASE = 0x10000000;

fn void main() @export("main") {
  Uart* uart0 = (Uart*)UART0_BASE;               // Create pointer to UART 0
	uart0.fcr = uart::UARTFCR_FFENA;               // Enable FIFO
  uart0.puts("Hello World!\n");                  // Write the string to the UART
  while (1);                                     // Loop forever to prevent program from ending
}

module uart;

const UARTFCR_FFENA = 0x01;                  // UART FIFO Control Register enable bit
const UARTLSR_THRE = 0x20;                   // UART Line Status Register Transmit Hold Register Empty bit

struct Uart {
	char dr;                                   // UART Data Register
	char filler1;
	char fcr;                                  // FIFO Control Register
	char filler2;
	char filler3;
	char lsr;                                  // Line Status Register
}

fn bool Uart.uart_ff_thr_empty(Uart* this) {
	return (bool)($$volatile_load(&this.lsr) & UARTLSR_THRE);
}

fn void Uart.putc(Uart* this, char c) {
  while (!this.uart_ff_thr_empty());         // Wait until the FIFO holding register is empty
  $$volatile_store(&this.dr, c);             // Write character to transmit register
}

fn void Uart.puts(Uart* this, char *str) {
  while (*str) {                             // Loop until value at string pointer is zero
    this.putc(*str++);                       // Write the character and increment pointer
  }
}

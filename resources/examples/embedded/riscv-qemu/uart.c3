module uart;

const UARTFCR_FFENA = 0x01;                  // UART FIFO Control Register enable bit
const UARTLSR_THRE = 0x20;                   // UART Line Status Register Transmit Hold Register Empty bit

struct Uart
{
	char dr;                                   // UART Data Register
	char filler1;
	char fcr;                                  // FIFO Control Register
	char filler2;
	char filler3;
	char lsr;                                  // Line Status Register
}

fn bool Uart.uart_ff_thr_empty(Uart* this)
{
	return (bool)(@volatile_load(this.lsr) & UARTLSR_THRE);
}

fn void Uart.putc(Uart* this, char c)
{
	while (!this.uart_ff_thr_empty());         // Wait until the FIFO holding register is empty
	@volatile_store(this.dr, c);             // Write character to transmit register
}

fn void Uart.puts(Uart* this, char *str)
{
	while (*str)                // Loop until value at string pointer is zero
    {
		this.putc(*str++);      // Write the character and increment pointer
	}
}

module std::core::mem;

<*
 @param #x : "The variable or dereferenced pointer to load."
 @return "The value of the variable"

 @require $defined(&#x) : "This must be a variable or dereferenced pointer"
*>
macro @volatile_load(#x) @builtin
{
	return $$volatile_load(&#x);
}

<*
 @param #x : "The variable or dereferenced pointer to store to."
 @param value : "The value to store."
 @return "The value stored"

 @require $defined(&#x) : "This must be a variable or dereferenced pointer"
 @require $defined(#x = value) : "The value doesn't match the variable"
*>
macro @volatile_store(#x, value) @builtin
{
	return $$volatile_store(&#x, ($typeof(#x))value);
}

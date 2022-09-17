// #target: macos-x64
module brainfk;
import std::io;

const BF_MEM = 30000;
char[BF_MEM] memory;

fault InterpretError
{
	INTEPRET_FAILED
}

fn void! print_error(usize pos, char[] err)
{
	io::printfln("Error at %s: %s", pos, err);
	return InterpretError.INTEPRET_FAILED!;
}

fn void! brainf(char[] program)
{
	usize sp = 0;
	usize mem = 0;
	while (sp < program.len)
	{
		char c = program[sp++];
		switch (c)
		{
			case '<':
				if (!mem) return print_error(sp, "Memory underflow");
				mem--;
			case '>':
				if (mem == BF_MEM - 1) return print_error(sp, "Memory overflow");
				mem++;
			case '+':
				memory[mem]++;
			case '-':
				memory[mem]--;
			case '.':
				io::putchar(memory[mem]);
				io::stdout().flush();
			case ',':
				memory[mem] = io::stdin().getc()!!;
			case '[':
				usize indent = 1;
				if (memory[mem]) continue;
				usize start = sp - 1;
				while (indent)
				{
					if (sp == program.len) return print_error(start, "No matching ']'");
					switch (program[sp++])
					{
						case ']': indent--;
						case '[': indent++;
						default: break;
					}
				}
			case ']':
				if (!memory[mem]) continue;
				usize start = sp--;
				usize indent = 1;
				while (indent)
				{
					if (!sp) return print_error(start, "No matching '['");
					switch (program[--sp])
					{
						case ']': indent++;
						case '[': indent--;
						default: break;
					}
				}
				sp++;
			default:
				break;
		}
	}
}
fn void! main()
{
	char[] program = `
	++++[>+++++<-]>[<+++++>-]+<+[
        >[>+>+<<-]++>>[<<+>>-]>>>[-]++>[-]+
        >>>+[[-]++++++>>>]<<<[[<++++++++<++>>-]+<.<[>----<-]<]
        <<[>>>>>[>>>[-]+++++++++<[>-<-]+++++++++>[-[<->-]+[<<<]]<[>+<-]>]<<-]<<-
    ]`;
    brainf(program)?;
}
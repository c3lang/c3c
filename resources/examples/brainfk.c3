module brainfk;
import std::io;

const BF_MEM = 30000;
char[BF_MEM] memory;

faultdef INTEPRET_FAILED;

fn void? print_error_type_at(sz pos, String err)
{
	io::printfn("Error at %s: %s", pos, err);
	return INTEPRET_FAILED~;
}

fn void? brainf(String program)
{
	sz sp = 0;
	sz mem = 0;
	while (sp < program.len)
	{
		char c = program[sp++];
		switch (c)
		{
			case '<':
				if (!mem) return print_error_type_at(sp, "Memory underflow");
				mem--;
			case '>':
				if (mem == BF_MEM - 1) return print_error_type_at(sp, "Memory overflow");
				mem++;
			case '+':
				memory[mem]++;
			case '-':
				memory[mem]--;
			case '.':
				io::putchar(memory[mem]);
				io::stdout().flush()!;
			case ',':
				memory[mem] = io::stdin().read_byte()!!;
			case '[':
				sz indent = 1;
				if (memory[mem]) continue;
				sz start = sp - 1;
				while (indent)
				{
					if (sp == program.len) return print_error_type_at(start, "No matching ']'");
					switch (program[sp++])
					{
						case ']': indent--;
						case '[': indent++;
						default: break;
					}
				}
			case ']':
				if (!memory[mem]) continue;
				sz start = sp--;
				sz indent = 1;
				while (indent)
				{
					if (!sp) return print_error_type_at(start, "No matching '['");
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
fn void main()
{
	String program = `
	++++[>+++++<-]>[<+++++>-]+<+[
        >[>+>+<<-]++>>[<<+>>-]>>>[-]++>[-]+
        >>>+[[-]++++++>>>]<<<[[<++++++++<++>>-]+<.<[>----<-]<]
        <<[>>>>>[>>>[-]+++++++++<[>-<-]+++++++++>[-[<->-]+[<<<]]<[>+<-]>]<<-]<<-
    ]`;
    brainf(program)!!;
}
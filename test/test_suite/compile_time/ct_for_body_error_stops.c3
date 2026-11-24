alias Fp = fn void();
alias Gp = fn int();

fn void runaway()
{
	$for var $i = 0; $i < 1; $i--:
		{
			Fp f = fn() => 0; // #error: 'int' cannot implicitly be converted to 'void'
		}
	$endfor
}

fn int valid()
{
	int s;
	$for var $i = 0; $i < 3; $i++:
		{
			Gp g = fn() => $i;
			s += g();
			$if $defined(nope()):
				nope();
			$endif
		}
	$endfor
	return s;
}

fn int main()
{
	$for var $i = 1; $i < 8; ++$i :
	$endfor

	$for var $i = 0; $i < 8; ++$i :
	$endfor

	const int[*] ABC = { 1, 2, 3 };
    $foreach $i, $val : ABC:
	$endforeach
	$foreach $i, $val : ABC:
	$endforeach

	return 0;
}
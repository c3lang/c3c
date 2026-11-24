fn int a() => 1;

fn void if_ct_if(int x)
{
	if (x) $if true: // #error: An 'if' statement may not be followed by a raw 'defer'
		defer {}
	$endif
}

fn void if_ct_switch(int x)
{
	if (x) $switch 1: // #error: An 'if' statement may not be followed by a raw 'defer'
		$case 1: defer a();
	$endswitch
}

fn void if_ct_for(int x)
{
	if (x) $for var $i = 0; $i < 1; $i++: // #error: An 'if' statement may not be followed by a raw 'defer'
		defer {}
	$endfor
}

fn void while_ct_if(int x)
{
	while (x) $if true: // #error: Looping over a raw 'defer' is not allowed
		defer {}
	$endif
}

fn void for_ct_if(int x)
{
	for (;x;) $if true: // #error: Looping over a raw 'defer' is not allowed
		defer {}
	$endif
}

fn void foreach_raw()
{
	foreach (i : (int[2]){}) defer {} // #error: Looping over a raw 'defer' is not allowed
}

fn void foreach_r_raw()
{
	foreach_r (i : (int[2]){}) defer {} // #error: Looping over a raw 'defer' is not allowed
}

fn void foreach_ct_if()
{
	foreach (i : (int[2]){}) $if true: // #error: Looping over a raw 'defer' is not allowed
		defer {}
	$endif
}

fn void if_ct_if_failing(int x)
{
	if (x) $if true:
		defer {}
		nope(); // #error: 'nope' could not be found
	$endif
}

fn void valid(int x)
{
	if (x) { $if true: defer {} $endif }
	if (x) $if true: a(); $endif
	while (x) $if true: x--; $endif
	foreach (i : (int[2]){}) { defer {} }
	foreach (i : (int[2]){}) $if true: x += i; $endif
	switch (x)
	{
		case 1: $if true: defer {} $endif
	}
}

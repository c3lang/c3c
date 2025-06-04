import std;

fn void main()
{
	int[] $nums = { 0, 1, 2 };
	var $list = { ...$nums, 3 };
	$foreach $n : $list :
		$echo $n;
	$endforeach
}

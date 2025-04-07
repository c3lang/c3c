module float_ct;

fn void float_ct() @test
{
	var $input = 3e25;
    var $temp = (int128)$input;
    var $temp2 = (double)$temp;
	test::eq_approx($temp2, 3e25);

	var $input2 = -3e25;
    var $temp3 = (int128)$input2;
    var $temp4 = (double)$temp3;
	test::eq_approx($temp4, -3e25);
}
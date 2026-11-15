module defined_typefrom_missing;
import std::io;

struct DtmFoo { int x; }
const String DTM_NAME = "DtmFoo";

fn void defined_typefrom_names() @test
{
	var $Tt = double;
	$assert $defined($Typefrom("DtmFoo"));
	$assert $defined($Typefrom("int"));
	$assert $defined($Typefrom("defined_typefrom_missing::DtmFoo"));
	$assert $defined($Typefrom("std::io::File"));
	$assert $defined($Typefrom("$Tt"));
	$assert $defined($Typefrom(DTM_NAME));
	$assert $defined($Typefrom(int::typeid));
	$assert !$defined($Typefrom("DtmNope"));
	$assert !$defined($Typefrom("defined_typefrom_missing::DtmNope"));
	$assert !$defined($Typefrom("dtm_no_such_module::DtmFoo"));
	$assert !$defined($Typefrom("$DtmNope"));
	$assert !$defined($Typefrom("DtmNope")[2]);
	$Typefrom("$Tt") d = 2.5;
	assert(d == 2.5);
}

module void_optional_ct_macro;

faultdef VOCM_FAIL;

macro void? ct_only($x) { }
macro void? ct_only_fails($x)
{
	$if $x:
		return VOCM_FAIL~;
	$endif
}
macro void? runtime_param(x) { }
macro int? ct_value($x) { return $x; }

fn void? rethrow_ct_only() { ct_only(1)!; }
fn void? rethrow_ct_only_fails() { ct_only_fails(true)!; }

fn void void_optional_macro_with_ct_params_keeps_optional() @test
{
	runtime_param(1)!!;
	ct_only(1)!!;
	assert(@ok(rethrow_ct_only()));
	assert(!@catch(ct_only(1)));
}

fn void void_optional_macro_with_ct_params_can_fail() @test
{
	assert(@catch(ct_only_fails(true)) == VOCM_FAIL);
	assert(!@catch(ct_only_fails(false)));
	assert(@catch(rethrow_ct_only_fails()) == VOCM_FAIL);
}

fn void optional_macro_with_ct_params_keeps_optional_value() @test
{
	assert(ct_value(3)!! == 3);
	assert(!@catch(ct_value(3)));
	int? v = ct_value(4);
	assert(v!! == 4);
}

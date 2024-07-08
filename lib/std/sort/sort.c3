module std::sort;


macro usz @len_from_list(&list)
{
	$if $defined(list.len()):
		return list.len();
	$else
		return list.len;
	$endif
}

macro bool @is_sortable(#list)
{
	$switch
		$case !$defined(#list[0]):
			return false;
		$case !$defined(#list.len):
			return false;
		$case $and($defined(&#list[0]) && !types::is_same($typeof(&#list[0]), $typeof(#list[0])*)):
			return false;
		$default:
			return true;
	$endswitch;
}

macro bool @is_cmp_fn(#cmp, #list)
{
	var $Type = $typeof(#cmp);
	$switch
	    $case $or($Type.kindof != FUNC, $Type.returns.kindof != SIGNED_INT): return false;
		$case $defined(#cmp(#list[0], #list[0])): return true;
		$case $defined(#cmp(&&(#list[0]), &&(#list[0]))): return true;
		$default: return false;
	$endswitch
}

macro bool @is_cmp_key_fn(#key_fn, #list)
{
	$switch
	    $case $typeof(#key_fn).kindof != FUNC: return false;
	    $case $typeof(#key_fn).returns.kindof != UNSIGNED_INT: return false;
		$case $defined(#key_fn(#list[0])): return true;
		$case $defined(#key_fn(&&(#list[0]))): return true;
		$default: return false;
	$endswitch
}
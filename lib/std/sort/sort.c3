module std::sort;


macro usz @len_from_list(&list)
{
	$if $defined(list.len()):
		return list.len();
	$else
		return list.len;
	$endif
}

macro bool @is_comparer(#cmp, #list)
{
	var $Type = $typeof(#cmp);
	$switch
	    $case $or($Type.kindof != FUNC, $Type.returns.kindof != SIGNED_INT): return false;
		$case $defined(#cmp(#list[0], #list[0])): return true;
		$case $defined(#cmp(&&(#list[0]), &&(#list[0]))): return true;
		$default: return false;
	$endswitch
}
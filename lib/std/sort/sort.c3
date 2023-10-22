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
	var $params = $typeof(#cmp).params;
	$if $params.len != 2:
		return false;
	$else
		$if $params[0] != $params[1]:
			return false;
		$else
			var $element = $typeof(#list[0]).typeid;
			$switch
				$case $element == $params[0]:
					return true;
				$case $and($params[0].kindof == POINTER, $params[0].inner == $element):
					return true;
				$default:
					return false;
			$endswitch
		$endif
	$endif
}
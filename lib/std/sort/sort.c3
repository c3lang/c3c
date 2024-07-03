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
	$if $and($Type.kindof == FUNC, $Type.returns.kindof == SIGNED_INT):
		var $params = $Type.params;
		$if $params.len != 2:
			return false;
		$else
			$if $params[0] != $params[1]:
				return false;
			$else
				var $element = @typeid(#list[0]);
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
	$else
		return false;
	$endif
}

macro bool @is_key(#item)
{
	var $key_type = $typeof(#item);
	$switch
		$case $key_type == @typeid(char):
			return true;
		$case $key_type == @typeid(ushort):
			return true;
		$case $key_type == @typeid(uint):
			return true;
		$case $key_type == @typeid(ulong):
			return true;
		$case $key_type == @typeid(ichar):
			return true;
		$case $key_type == @typeid(short):
			return true;
		$case $key_type == @typeid(int):
			return true;
		$case $key_type == @typeid(long):
			return true;
		$case $key_type == @typeid(bool):
			return true;
		$default:
			return false;
	$endswitch
}

macro bool @is_keyer(#key, #list)
{
	var $Type = $typeof(#key);
	$switch
	    $case $Type.kindof != FUNC: return false;
		$case $and($defined(#key(#list[0])), @is_key(#key(#list[0]))): 
			return true;
		$case $and($defined(#key(&&(#list[0]))), @is_key(#key(#list[0]))):
			return true;
		$default: return false;
	$endswitch
}

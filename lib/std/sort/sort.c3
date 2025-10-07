module std::sort;

macro bool @list_is_by_ref(#list) @const
{
	return $kindof(#list) == SLICE ||| ($kindof(#list) == POINTER &&& $kindof(*#list) != SLICE);
}

<*
 @require @list_is_by_ref(#list) : "Expected the list to be passed by ref or be a slice"
*>
macro bool @is_sortable(#list) @const
{
	$switch:
		$case $kindof(#list) == SLICE:
			return true;
		$case !$defined(*#list):
			return false;
		$case !$defined((*#list)[0]):
			return false;
		$case !$defined(lengthof(*#list)):
			return false;
		$case $defined(&(*#list)[0]) &&& $typeof(&(*#list)[0]) != $typeof((*#list)[0])*:
			return false;
		$default:
			return true;
	$endswitch
}

macro bool @is_any_sortable(#list) @const
{
	$switch $kindof(#list):
		$case SLICE:
			return true;
		$case POINTER:
			return @is_sortable(#list);
		$default:
			return $defined(#list[0]) &&& $defined(lengthof(#list))
				&&& !($defined(&#list[0]) &&& $typeof(&#list[0]) != $typeof(#list[0])*);
	$endswitch
}

macro bool @is_valid_context(#cmp, #context)
{
	return @is_valid_macro_slot(#cmp) || @is_empty_macro_slot(#context);
}


<*
 @require @list_is_by_ref(#list) : "Expected the list to be passed by ref or be a slice"
*>
macro bool @is_valid_cmp_fn(#cmp, #list, #context) @const
{
	var $Type = $typeof(#cmp);
	var $no_context = @is_empty_macro_slot(#context);
	$switch:
		$case @is_empty_macro_slot(#cmp): return true;
		$case $Type.kindof != FUNC ||| $Type.returns.kindof != SIGNED_INT: return false;
		$default:
			$if $kindof(#list) == SLICE:
				$switch:
					$case $defined(#cmp((#list)[0], (#list)[0], #context)): return true;
					$case $defined(#cmp((#list)[0], (#list)[0])): return $no_context;
					$case $defined(#cmp(&(#list)[0], &(#list)[0], #context)): return true;
					$case $defined(#cmp(&(#list)[0], &(#list)[0])): return  $no_context;
					$default: return false;
				$endswitch
			$else
				$switch:
					$case $defined(#cmp((*#list)[0], (*#list)[0], #context)): return true;
					$case $defined(#cmp((*#list)[0], (*#list)[0])): return $no_context;
					$case $defined(#cmp(&(*#list)[0], &(*#list)[0], #context)): return true;
					$case $defined(#cmp(&(*#list)[0], &(*#list)[0])): return  $no_context;
					$default: return false;
				$endswitch
			$endif
	$endswitch
}

macro bool @is_any_valid_cmp_fn(#cmp, #list, #context) @const
{
	var $Type = $typeof(#cmp);
	var $no_context = @is_empty_macro_slot(#context);
	$switch:
		$case @is_empty_macro_slot(#cmp): return true;
		$case $Type.kindof != FUNC ||| $Type.returns.kindof != SIGNED_INT: return false;
		$default:
			$if $kindof(#list) != POINTER:
				$switch:
					$case $defined(#cmp((#list)[0], (#list)[0], #context)): return true;
					$case $defined(#cmp((#list)[0], (#list)[0])): return $no_context;
					$case $defined(#cmp(&(#list)[0], &(#list)[0], #context)): return true;
					$case $defined(#cmp(&(#list)[0], &(#list)[0])): return  $no_context;
					$default: return false;
				$endswitch
			$else
				$switch:
					$case $defined(#cmp((*#list)[0], (*#list)[0], #context)): return true;
					$case $defined(#cmp((*#list)[0], (*#list)[0])): return $no_context;
					$case $defined(#cmp(&(*#list)[0], &(*#list)[0], #context)): return true;
					$case $defined(#cmp(&(*#list)[0], &(*#list)[0])): return  $no_context;
					$default: return false;
				$endswitch
			$endif
	$endswitch
}

<*
 @require @list_is_by_ref(#list) : "Expected the list to be passed by ref or be a slice"
*>
macro bool @is_cmp_key_fn(#key_fn, #list) @const
{
	$switch:
		$case @is_empty_macro_slot(#key_fn): return true;
		$case $kindof(#key_fn) != FUNC: return false;
		$case $typeof(#key_fn).returns.kindof != UNSIGNED_INT: return false;
		$default:
			$if $kindof(#list) == SLICE:
				$switch:
					$case $defined(#key_fn((#list)[0])): return true;
					$case $defined(#key_fn(&&((#list)[0]))): return true;
					$default: return false;
				$endswitch
			$else
				$switch:
					$case $defined(#key_fn((*#list)[0])): return true;
					$case $defined(#key_fn(&&((*#list)[0]))): return true;
					$default: return false;
				$endswitch
			$endif
	$endswitch

}
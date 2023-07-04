module std::sort;

macro bool is_searchable(list)
{
	return $checks(list[0]) && ($checks(list.len) || $checks(list.len()));
}

macro usz @len_from_list(&list)
{
	$if $checks(list.len()):
		return list.len();
	$else
		return list.len;
	$endif
}

macro bool is_comparer(cmp, list)
{
	return $checks(int i = cmp(list[0], list[0]))
		|| $checks(int i = cmp(&list[0], &list[0]));
}
// Copyright (c) 2021-2022 Christoffer Lerno and contributors. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::core::builtin;

/**
 * @require types::is_comparable_value(a) && types::is_comparable_value(b)
 **/
macro less(a, b) @builtin
{
	$if ($defined(a.less)):
		return a.less(b);
	$elif ($defined(a.compare_to)):
		return a.compare_to(b) < 0;
	$else:
		return a < b;
	$endif;
}

/**
 * @require types::is_comparable_value(a) && types::is_comparable_value(b)
 **/
macro less_eq(a, b) @builtin
{
	$if ($defined(a.less)):
		return !b.less(a);
	$elif ($defined(a.compare_to)):
		return a.compare_to(b) <= 0;
	$else:
		return a <= b;
	$endif;
}

/**
 * @require types::is_comparable_value(a) && types::is_comparable_value(b)
 **/
macro greater(a, b) @builtin
{
	$if ($defined(a.less)):
		return b.less(a);
	$elif ($defined(a.compare_to)):
		return a.compare_to(b) > 0;
	$else:
		return a > b;
	$endif;
}

/**
 * @require types::is_comparable_value(a) && types::is_comparable_value(b)
 **/
macro greater_eq(a, b) @builtin
{
	$if ($defined(a.less)):
		return !a.less(b);
	$elif ($defined(a.compare_to)):
		return a.compare_to(b) >= 0;
	$else:
		return a >= b;
	$endif;
}

/**
 * @require types::is_equatable_value(a) && types::is_equatable_value(b) `values must be equatable`
 **/
macro bool equals(a, b) @builtin
{
	$if ($defined(a.equals)):
		return a.equals(b);
	$elif ($defined(a.compare_to)):
		return a.compare_to(b) == 0;
	$elif ($defined(a.less)):
		return !a.less(b) && !b.less(a);
	$else:
		return a == b;
	$endif;
}

macro min(x, ...) @builtin
{
    $if ($vacount == 1):
        return less(x, $vaarg(0)) ? x : $vaarg(0);
    $else:
        $typeof(x) result = x;
        $for (var $i = 0; $i < $vacount; $i++):
            if (less($vaarg($i), result))
            {
                 result = $vaarg($i);
            }
        $endfor;
        return result;
    $endif;
}

macro max(x, ...) @builtin
{
    $if ($vacount == 1):
        return greater(x, $vaarg(0)) ? x : $vaarg(0);
    $else:
        $typeof(x) result = x;
        $for (var $i = 0; $i < $vacount; $i++):
            if (greater($vaarg($i), result))
            {
                 result = $vaarg($i);
            }
        $endfor;
        return result;
    $endif;
}


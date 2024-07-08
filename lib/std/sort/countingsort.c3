module std::sort;
import std::sort::is;
import std::sort::cs;
import std::sort::qs;

/**
 * Sort list using the counting sort algorithm.
 * @require $defined(list[0]) && $defined(list.len) "The list must be indexable and support .len or .len()"
 * @require $or(@typeid(key) == void*.typeid, @is_keyer(key, list)) "Expected a transformation function which returns a key-like type, think integral types"
 **/
macro countingsort(list, key = null) @builtin
{
	var $Type = $typeof(list);
	var $KeyFuncType = $typeof(key);
	usz len = sort::@len_from_list(list);
	cs::csort(<$Type, $KeyFuncType>)(list, 0, len, key, ~((uint)0));
}

macro insertionsort_indexed(list, start, end, cmp = null) @builtin
{
	var $Type = $typeof(list);
	var $CmpFuncType = $typeof(cmp);
	is::isort(<$Type, $CmpFuncType>)(list, (usz)start, (usz)end, cmp);
}

macro quicksort_indexed(list, start, end, cmp = null) @builtin
{
	var $Type = $typeof(list);
	var $CmpFuncType = $typeof(cmp);
	qs::qsort(<$Type, $CmpFuncType>)(list, (isz)start, (isz)(end-1), cmp);
}

module std::sort::cs(<Type, Keyer>);

def Counts = ulong[256] @private;
def Ranges = ulong[257] @private;
def Indexs = char[256] @private;
def ElementType = $typeof(Type{}[0]);
//def KeyerReturnTypeid = $typeof(Keyer.returns); //Keyer{}(Keyer.params[0]{})
def KeyerReturnType = $evaltype(Keyer.returns.qnameof);
def CmpCallback     = fn bool(ElementType*, ElementType*);

fn void csort(Type list, usz low, usz high, Keyer key, uint byte_idx)
{
	var $no_key = Keyer.typeid == void*.typeid;
    var $key_by_value = $and(!$no_key, Keyer.params[0] == @typeid(list[0]));
    var $list_has_get_ref = $defined(list.get_ref(0));

    byte_idx = byte_idx >= KeyerReturnType.sizeof ? KeyerReturnType.sizeof-1 : byte_idx;
	
	if (high <= low) {
		return;
	}

	Counts counts;
	Ranges ranges;
	Indexs indexs;

	KeyerReturnType mn = ~((KeyerReturnType)0);
	KeyerReturnType mx = (KeyerReturnType)0;

	char last_key = 0;
	char keys_ordered = 1;

	for (usz i = 0; i < 256; i++) {
		counts[i] = 0;
	}

	for (usz i = low; i < high; i++) {
		$if $list_has_get_ref:
			$if $key_by_value:
				KeyerReturnType k = key(*list.get_ref(i));
			$else
				KeyerReturnType k = key(list.get_ref(i));
			$endif;


		$else
			$if $key_by_value:
				KeyerReturnType k = key(list[i]);
			$else
				KeyerReturnType k = key(&list[i]);
			$endif;
		$endif

		char key_byte = (char)((k >> (byte_idx*8)) & 0xff);
		++counts[key_byte];

		mn = k < mn ? k : mn;
		mx = k > mx ? k : mx;

		keys_ordered = keys_ordered & (char)(key_byte >= last_key); 
		last_key     = key_byte;
	}

	KeyerReturnType diff = mx-mn;
	if (diff == 0) {
		return;
	}

	ushort parition_count = 0;
	usz total = 0;
	for (usz i = 0; i < 256; i++) {
		usz count = counts[i];
		indexs[parition_count] = (char)i;
		parition_count += (ushort)(count > 0);
		counts[i] = total;
		ranges[i] = total;
		total += count;
	}
	ranges[256] = total;
	
	if (!keys_ordered) {
		usz sorted_count = 0;
		//ElementType* first = list.first();
		do {
			for (usz x = 0; x < 256; x++) {
				usz s = counts[x];
				usz e = ranges[x + 1];

				sorted_count += (e - s);
				for (; s < e; s++) {
					$if $list_has_get_ref:
						$if $key_by_value:
							KeyerReturnType k = key(*list.get_ref(low+s));
						$else
							KeyerReturnType k = key(list.get_ref(low+s));
						$endif;
						char k_idx = (char)(k >> (byte_idx*8));
						usz          target_idx = counts[k_idx];

						@swap(list[low+s], list[low+target_idx]);
						counts[k_idx] += 1;
					$else
						$if $key_by_value:
							KeyerReturnType k = key(list[low+s]);
						$else
							KeyerReturnType k = key(&list[low+s]);
						$endif;
						char k_idx = (char)(k >> (byte_idx*8));
						usz          target_idx = counts[k_idx];

						@swap(list[low+s], list[low+target_idx]);
						counts[k_idx] += 1;
					$endif;
				}
			}
		} while (sorted_count < ranges[256]);
	}

	if (byte_idx) {
		for (usz p = 0; p < parition_count; ++p) {
			usz i = indexs[p];
			usz	start_offset = ranges[i];
			usz	end_offset   = ranges[i + 1];

			usz	items        = end_offset - start_offset;

			if (items <= 32) {
				insertionsort_indexed(list, low+start_offset, low+end_offset, fn bool (ElementType* lhs, ElementType* rhs) {
					return Keyer{}(lhs) < Keyer{}(rhs);
				});
			} else if (items <= 128) {
				quicksort_indexed(list, low+start_offset, low+end_offset, fn long (ElementType* lhs, ElementType* rhs) {
					return (long)(Keyer{}(lhs) - Keyer{}(rhs));
				});
			} else {
				csort(list, low+start_offset, low+end_offset, key, byte_idx-1);
			}
		}
	}
}

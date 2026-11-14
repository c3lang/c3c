module vasplat_ranges;

macro int[*] @splat_ends(...) => { 7, ...$vaarg[0..1], 8 };
macro int[*] @splat_len(...) => { 7, ...$vaarg[0:2], 8 };
macro int[*] @splat_to_last(...) => { 7, ...$vaarg[..^1], 8 };
macro int[*] @splat_from_end(...) => { 7, ...$vaarg[^2..], 8 };
macro int[*] @splat_none(...) => { 7, ...$vaarg[^0..], 8 };
macro int[*] @splat_empty(...) => { 7, ...$vaarg[1..0], 8 };

fn void vasplat_ranges() @test
{
	assert(@splat_ends(1, 2) == { 7, 1, 2, 8 });
	assert(@splat_len(1, 2) == { 7, 1, 2, 8 });
	assert(@splat_to_last(1, 2) == { 7, 1, 2, 8 });
	assert(@splat_from_end(1, 2) == { 7, 1, 2, 8 });
	assert(@splat_none(1, 2) == { 7, 8 });
	assert(@splat_empty(1, 2) == { 7, 8 });
}

module test;

typedef MyVec = float[<4>];

// The element types match: this works today and must keep working.
fn void same_element_array_to_distinct_vector()
{
	float[4] a = { 1, 2, 3, 4 };
	MyVec v = (MyVec)a;
}

// The element types differ, so the array is first converted to a float vector.
fn void other_element_array_to_distinct_vector()
{
	int[4] a = { 1, 2, 3, 4 };
	MyVec v = (MyVec)a;
}

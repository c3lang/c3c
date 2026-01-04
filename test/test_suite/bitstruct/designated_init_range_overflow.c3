enum Row : inline int { A, B, C, D, E, F }

int[3][6] m = {
    [Row.E] = {
        [0..3] = 1, // #error: The index must be less than the array length (which was 3)
    },
    [Row.F] = {
        [1] = 2,
    },
};

fn int main()
{
	(void)m;
	return 0;
}
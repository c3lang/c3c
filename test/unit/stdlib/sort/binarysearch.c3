module sort_test @test;
import std::sort;

struct BinarySearchTest
{
    int[] data;
    int x;
    int index;
}

fn void binarysearch()
{
    BinarySearchTest[] tcases = {
        { {}, 0, 0 },
        { {1, 2, 3}, 1, 0 },
        { {1, 2, 3}, 2, 1 },
        { {1, 2, 3}, 3, 2 },
        { {1, 2, 3}, 4, 3 },
        { {10, 20, 30}, 14, 1 },
        { {10, 20, 30}, 26, 2 },
    };

    foreach (tc : tcases)
    {
        usz idx = sort::binarysearch(tc.data, tc.x);
        assert(idx == tc.index, "%s: got %d; want %d", tc.data, idx, tc.index);

        usz cmp_idx = sort::binarysearch(tc.data, tc.x, &sort::cmp_int_ref);
        assert(cmp_idx == tc.index, "%s: got %d; want %d", tc.data, cmp_idx, tc.index);

        usz cmp_idx2 = sort::binarysearch(tc.data, tc.x, &sort::cmp_int_value);
        assert(cmp_idx2 == tc.index, "%s: got %d; want %d", tc.data, cmp_idx2, tc.index);

        usz cmp_idx3 = sort::binarysearch(tc.data, tc.x, fn int(int a, int b) => a - b);
        assert(cmp_idx3 == tc.index, "%s: got %d; want %d", tc.data, cmp_idx2, tc.index);
    }
}
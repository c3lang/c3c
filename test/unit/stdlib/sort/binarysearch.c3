module binarysearch_test @test;
import std::sort::binarysearch;

struct SearchTest
{
    int[] data;
    int x;
    int index;
}

fn void search()
{
    SearchTest[] tcases = {
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
        usz idx = binarysearch::search(tc.data, tc.x);
        assert(idx == tc.index, "%s: got %d; want %d", tc.data, idx, tc.index);

        usz cmp_idx = binarysearch::cmp_search(tc.data, tc.x, &cmp_int);
        assert(cmp_idx == tc.index, "%s: got %d; want %d", tc.data, cmp_idx, tc.index);

        usz cmp_idx2 = binarysearch::cmp_search(tc.data, tc.x, &cmp_int2);
        assert(cmp_idx2 == tc.index, "%s: got %d; want %d", tc.data, cmp_idx2, tc.index);

        usz cmp_idx3 = binarysearch::cmp_search(tc.data, tc.x, fn int(int a, int b) => a - b);
        assert(cmp_idx3 == tc.index, "%s: got %d; want %d", tc.data, cmp_idx2, tc.index);
    }

}

module binarysearch_test;

fn int cmp_int(void* x, void* y) {
    return *(int*)x - *(int*)y;
}

fn int cmp_int2(int x, int y) {
    return x - y;
}


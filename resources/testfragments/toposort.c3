module topologicalsort;

extern fn void printf(char* x, ...);

struct InputPair
{
    int value;
    int successor;
}

struct Entry
{
    int value;
    Entry* next;
}

struct TopoList
{
    int count;
    Entry* next;
}


fn void sort(InputPair[] pairs, uint elements)
{
    InputPair[] result = mem::alloc_array(InputPair, pairs.len);
    TopoList* top = mem::alloc_array(TopoList, elements);
    for (int i = 0; i < pairs.len; i++)
    {
        InputPair pair = pairs[i];
        assert(pair.value >= 0 && pair.value < elements);
        assert(pair.successor >= 0 && pair.successor < elements);
        top[pair.successor].count++;
        Entry* successor_entry = mem::new(Entry, { pair.successor, null });
        Entry** next_ref = &top[pair.value].next;
        while (*next_ref)
        {
            next_ref = &(*next_ref).next;
        }
        *next_ref = successor_entry;
    }
    int[] intout = mem::alloc_array(int, elements);
    int count = 0;
    while LOOP: (1)
    {
        for (int i = 1; i < elements; i++)
        {
            if (top[i].count == 0)
            {
                intout[count++] = i;
                Entry *next = top[i].next;
                while (next)
                {
                    top[next.value].count--;
                    next = next.next;
                }
                top[i].count = -1;
                continue LOOP;
            }
        }
        break;
    }
    printf("Got %d elements.\n", count);
    for (int i = 0; i < count; i++)
    {
        printf("%d\n", intout[i]);
    }
}

fn void main()
{
    InputPair[10] pairs = {
        { 9, 2 },
        { 3, 7 },
        { 7, 5 },
        { 5, 8 },
        { 8, 6 },
        { 4, 6 },
        { 1, 3 },
        { 7, 4 },
        { 9, 5 },
        { 2, 8 },
    };
    sort(&pairs, 10);
}
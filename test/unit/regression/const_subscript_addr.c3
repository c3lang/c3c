module const_subscript_addr_test @test;

struct Entry
{
	int id;
	int val;
}

struct Registry
{
	int count;
	Entry* entries;
}

Entry[3] global_entries = {
	{ 10, 100 },
	{ 20, 200 },
	{ 30, 300 }
};

Registry global_reg = {
	.count = 3,
	.entries = &global_entries[0]
};

Entry* global_entry_ptr = &global_entries[1];

fn void test_const_subscript_addr()
{
	assert(global_reg.count == 3);
	assert(global_reg.entries != null);
	assert(global_reg.entries[0].id == 10);
	assert(global_reg.entries[0].val == 100);
	assert(global_reg.entries[1].id == 20);
	assert(global_reg.entries[1].val == 200);
	assert(global_entry_ptr != null);
	assert(global_entry_ptr.id == 20);
	assert(global_entry_ptr.val == 200);
}

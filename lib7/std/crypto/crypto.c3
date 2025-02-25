module std::crypto;

fn bool safe_compare(void* data1, void* data2, usz len)
{
	char match = 0;
	for (usz i = 0; i < len; i++)
	{
		match = match | (mem::@volatile_load(((char*)data1)[i]) ^ mem::@volatile_load(((char*)data2)[i]));
	}
	return match == 0;
}


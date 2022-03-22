fn void test1()
{
	do test1(); while(1); // #error: A do loop must use { } around its body
}


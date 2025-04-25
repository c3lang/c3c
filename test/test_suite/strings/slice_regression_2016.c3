import std;
fn void main()
{
	String str1;
	String str2;
	str1[..] = str2[..];
	test2();
}

fn void test1()
{
	String str1;
	String str2;
	str1[..] = str2; // #error: You cannot cast 'String' to 'char'
	str1[..] = str2[..];
}
typedef String16 = inline Char16[];

fn void test2()
{
	Char16[] str1;
	String16 str2;
	str1[..] = str2[..];
}
fn int main(String[] args)
{
	$defined(int a = 3.0);
	$defined(int[*] b); // #error: Inferred array types can only be used in declarations
}
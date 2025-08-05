fn void main()
{
	$echo $typeof(0X00.).nameof; // #error: Hex floating points must end with 'p' or 'P'
}

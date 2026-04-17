fn void main()
{
	$echo $typeof(0X00.)::name; // #error: Hex floating points must end with 'p' or 'P'
}

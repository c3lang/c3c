fn void main()
{
	$echo $Typeof(0X00.)::name; // #error: Hex floating points must end with 'p' or 'P'
}

fn void main()
{
	double[<65>] x; // #error: Vectors with bitsize over
	$echo $sizeof(x) * 8;
}

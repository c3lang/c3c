fn void main() 
{
	int value = 0;

	$debugscope:
	defer value += 2; 
	$enddebugscope

	value -= 2;
}
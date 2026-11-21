<* @param [in] s *>
fn void in_deref_not_pointee(String* s, int[]* sp)
{
	(*s)[0] = 'a';
	(*sp)[0] = 1;
}

<* @param [in] ap *>
fn void in_deref_array(int[3]* ap)
{
	(*ap)[0] = 1; // #error: 'in' parameters may not be assigned to
}

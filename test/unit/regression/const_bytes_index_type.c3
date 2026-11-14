module const_bytes_index_type;

typedef CbitByte = char;
typedef CbitSByte = ichar;

const ichar[*] CBIT_IC = x"ff01";
const CbitByte[*] CBIT_BY = (CbitByte[*])x"0aff";
const CbitSByte[*] CBIT_SB = (CbitSByte[*])x"80";
const char[*] CBIT_CH = x"ff";
const String CBIT_S = "abc";
const ichar* CBIT_ICP = (ichar*)"\xfe";

fn void const_bytes_index_type() @test
{
	$assert CBIT_IC[0] == -1;
	$assert CBIT_IC[^1] == 1;
	$assert $Typeof(CBIT_IC[0]) == ichar;
	$assert $Typeof(CBIT_BY[1]) == CbitByte;
	$assert (char)CBIT_BY[1] == 255;
	$assert $Typeof(CBIT_SB[0]) == CbitSByte;
	$assert (ichar)CBIT_SB[0] == -128;
	$assert CBIT_CH[0] == 255;
	$assert $Typeof(CBIT_S[1]) == char;
	$assert *CBIT_ICP == -2;
	ichar a = CBIT_IC[0];
	CbitByte b = CBIT_BY[1];
	CbitSByte c = CBIT_SB[0];
	ichar d = *CBIT_ICP;
	assert(a == -1 && (char)b == 255 && (ichar)c == -128 && d == -2);
}

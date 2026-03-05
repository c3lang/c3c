module test;

const BITS = 8;
fn int main()
{
 String $chars = "Abcd";
 char $from = 2;
 char[10] $bitmap = {};
$foreach  $c : $chars:
        int $offset = ($c - $from) / BITS;
        int $rem = ($c - $from) % BITS;
        uint128 $value = $bitmap[$offset]; // #error: is out of range
$endforeach
}

fn int main2()
{
 String $chars = "Abcd";
 char $from = 2;
 char[10] $bitmap = {};
$foreach  $c : $chars:
        int $offset = ($c - $from) / BITS;
        int $rem = ($c - $from) % BITS;
        uint128 $value = 1;
        $value |= 1ULL << $rem;
        $bitmap = $bitmap[:$offset] +++ $value +++ $bitmap[$offset+1..]; // #error: End index out of bounds
$endforeach
}
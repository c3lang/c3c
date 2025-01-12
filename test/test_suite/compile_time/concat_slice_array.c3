module test;

const BITS = 8;
fn int main()
{
 String $chars = "Abcd";
 char $from = 2;
 char[10] $bitmap = {};
$foreach ( $c : $chars)
        int $offset = ($c - $from) / BITS;
        int $rem = ($c - $from) % BITS;
        uint128 $value = $bitmap[$offset];
        $value |= 1u128 << $rem;
        $bitmap = $bitmap[:$offset] +++ $value +++ $bitmap[$offset+1..]; // #error: Only fully initialized arrays
$endforeach
}
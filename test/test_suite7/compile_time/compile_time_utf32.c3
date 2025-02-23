macro int get_utf32_codepoint_length(char $c) @const
{
    int $len;
    $switch
        $case !($c & 0x80):           $len = 1;
        $case ($c & 0xE0) == 0xC0:    $len = 2;
        $case ($c & 0xF0) == 0xE0:    $len = 3;
        $case ($c & 0xF8) == 0xF0:    $len = 4;
        $default:                     $len = -1;
    $endswitch
    return $len;
}

macro usz get_utf32_len_from_utf8(String $source) @const
{
    usz $len = 0;
    $for (var $i=0; $i < $source.len;)
        var $curlen = get_utf32_codepoint_length ($source[$i]);
        $i += $curlen;
        $len++;
    $endfor
    return $len;
}

macro Char32[] conv_utf8_to_char32($source) @const
{
    Char32 $codepoint;
    var $max = $source.len;

    var $char32len = get_utf32_len_from_utf8 ($source);
    Char32[] $chars = {};
    var $char_index = 0;

    $for (var $inx = 0; $inx < $max; )
        var $c = $source[$inx];
        var $len = get_utf32_codepoint_length($c);
        $switch ($len)
            $case 1: $codepoint = $c;
            $case 2: $codepoint = $c & 0x1F;
            $case 3: $codepoint = $c & 0x0F;
            $case 4: $codepoint = $c & 0x07;
            $default:
                $error("Error, invalid utf8 character in string");
                return;
        $endswitch

        $for (var $x = 1; $x < $len; ++$x)
            $c = $source[$inx + $x];
            $if ( ($c & 0xC0) != 0x80):
                $error("Error, invalid utf8 character in string");
                return;
            $endif
            $codepoint = ($codepoint << 6) | ($c & 0x3F);
        $endfor

        $chars = $chars +++ $codepoint;
        $inx += $len;
    $endfor
    return $chars;
}
import std;
fn void main(String[] args)
{
	Char32[] $chars = conv_utf8_to_char32("🐉 eats 🌎");
	$foreach($v : $chars)
		io::printn($v);
	$endforeach
    Char32[] chars = conv_utf8_to_char32 ("🐉 eats 🌎");
    io::printn(chars);
    foreach (c : chars)
    {
        io::printfn ("char32 :%x", c );
    }
}
// #target: macos-x64
module testing;
import std::io;

fn void main()
{
    String[] s1;
    String[] s2;
    deep_equal(s1, s2);
}

macro bool deep_equal(a, b)
{
    $switch ($typeof(a).kindof)
        $case SLICE:
            if (a.len != b.len) return false;
            foreach (i, x : a)
            {
                if (!deep_equal(x, b[i])) return false;
            }
            return true;
        $default:
            $assert(false); // #error: Compile time assert failed
    $endswitch
}
module testing;
import std::io;

fn void main()
{
    String[] s1;
    String[] s2;
    assert(false, "%s / %s", deep_print(s1), deep_print(s2)); // #error: This expression is of type
}

macro deep_print(a)
{
    var $Type = $typeof(a);
    $switch ($Type.kindof)
        $case ARRAY:
        $case SLICE:
            io::print("[");
            foreach (i, x : a)
            {
                if (i > 0) io::print(", ");
                deep_print(x);
            }
            io::print("]");
        $case STRUCT:
            io::print("{");
            $foreach ($i, $m : $typeof(a).membersof)
                if ($i > 0) io::print(", ");
                deep_print($m.get(a));
            $endforeach
            io::print("}");
        $default:
            $switch ($Type.nameof)
                $case "String":
                $case "DString":
                $case "WString":
                $case "ZString":
                    io::printf(`"%s"`, a);
                $default:
                    io::printf("%s", a);
            $endswitch
    $endswitch
}
int x;
$if x > 0:
$endif

$if 0:
    $assert false;
$endif

$if 1:
$else
$endif

$if 1:
$else
$else // #error: Expected the start of a global declaration here
$endif


$switch
$case true:
$case true:
$default:
$endswitch

$switch
$case true:
$case true:
$case true:
$default:
$endswitch


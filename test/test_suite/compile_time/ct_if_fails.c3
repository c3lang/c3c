int x;
$if x > 0:
$endif;

$if 0:
    $assert(false);
$endif;

$if 1:
$else:
$endif;

$if 1:
$else:
$else: // #error: Expected a top level declaration here.
$endif;


$if 1:
$elif 2:
$else:
$endif;

$if 1:
$elif 2:
$elif 3:
$else:
$endif;

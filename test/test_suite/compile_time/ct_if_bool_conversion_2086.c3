module test;

fn void main()
{
    int $a = 0;
    $if $a:
    $endif;
    int a @if(!$a);
    int a @if($a);
    int a @if((bool)$a);
}
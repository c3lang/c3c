fn int main()
{
    var $counter ;
    $if $defined($counter):
         $counter++; // #error: The compile time variable '$counter' must be assigned a starting value before it can be
    $endif
    return 0;
}
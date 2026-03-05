fn void main()
{
    foreach_r (void a : foo) // #error:  'void' is not a valid type for 'a', only runtime types with a known size may be used
    {
    }
}
// Issue #1824
fn void main()
{
  $if true:
  $else
  $else // #error: Expected '$endif'
  $endif
}
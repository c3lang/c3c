
// Check that 0 <= buf is ok.
<*
 @ensure return <= (usz)buf.len
 *>
fn usz foo(char[] buf) {
  if (buf.len == 0) return 0;
  // ... return some index into buf
  return (usz)buf.len;
}
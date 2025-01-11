module test;
// Issue #1811
bitstruct Image_Tag : uint
{
  char x1 : 24..31;
  char x2 : 16..23;
  char x3 : 8..15;
  char x4 : 0..7;
}

distinct Glyph_Format = CInt;
const Glyph_Format GLYPH_FORMAT_BITMAP = (Glyph_Format)Image_Tag {'b', 'i', 't', 's'};

fn void main() {
  // Error: Implicitly casting 'Image_Tag' to 'Glyph_Format' is not permitted
  Glyph_Format format = GLYPH_FORMAT_BITMAP;
  // Error: 'Glyph_Format' and 'Image_Tag' are different types and cannot be compared.
  assert(format == GLYPH_FORMAT_BITMAP);
}
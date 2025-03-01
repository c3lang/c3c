enum Baz : (String a, int z)
{
  BAR = { "123", 1, 3 }, // #error: You're adding too many values, only 2 associated
}

enum Baz2 : (String a, int z)
{
  BAR = { "123" }, // #error: Expected 2 associated values for this enum value
}
struct ParentType { int a; }
struct ChildType  { ParentType b;}
const ChildType ABC2 = {     };
float[] asd = { ...ABC2, 0x828dabe3efd81cfa, .c = 8 }; // #error: You’re using both named and unnamed values in the same initializer.
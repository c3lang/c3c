module struct2;

struct Blend_Map_Entry
{
  union vals {
   float[5] colour;
   double[2] point_Slope;
  }
}

Blend_Map_Entry a = { .vals = { .colour = { 1, 2, 3, 4, 5 } } };
Blend_Map_Entry b = { .vals = { .point_Slope = { 6, 7 } } };
Blend_Map_Entry c = { .vals.colour[2] = 1 };
Blend_Map_Entry d = { .vals.colour = { 1, 2, 3, 4, 5 } };

func void test(Blend_Map_Entry* foo)
{
}
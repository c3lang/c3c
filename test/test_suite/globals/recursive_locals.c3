struct List
{
  int x;
  List* next;
}

local const List A = { 7, &B };
local const List B = { 8, &A };

extern List d;

List c = { 7, &d };

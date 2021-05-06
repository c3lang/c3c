
struct List
{
  int x;
  List* next;
}

const List A = { 7, &B };
const List B = { 8, &A };

extern List d;

List c = { 7, &d };

int[3] abc;
int *bf3 = (&abc[4]) + 2; // #error: An index of '4' is out of range, a value between 0 and 2 was expected
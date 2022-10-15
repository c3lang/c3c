
int[-1] a;      // #error: An array may not have a negative
int[10-20] b;   // #error: An array may not have a negative
int[<-1>] c; // #error: A vector may not have a negative width
int[<10-20>] d; // #error: A vector may not have a negative width

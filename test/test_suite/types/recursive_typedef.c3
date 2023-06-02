def Number2 = Number1; // #error: Recursive definition of 'Number2'
def Number1 = Number2;

def Number = Number; // #error: Recursive definition of 'Number'


def Loop2 = Loop; // #error: Recursive definition of 'Loop2'
def Loop3 = Loop2;
def Loop = Loop3;

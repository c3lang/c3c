
def int = int; // #error: 'int' is a reserved keyword, try another name
def main = foo; // #error: 'main' is reserved and cannot be used as an alias.

def hello = int; // #error: uppercase letter
def hello = Foo; // #error: uppercase letter

def HELLO = int; // #error: uppercase letter
def HELLO = Foo; // #error: uppercase letter
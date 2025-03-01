
def Foo = int;

def hello1 = int; // #error: To alias a type, the alias
def hello2 = Foo; // #error: To alias a type, the alias

def HELLO1 = int; // #error: To alias a type, the alias
def HELLO2 = Foo; // #error: To alias a type, the alias
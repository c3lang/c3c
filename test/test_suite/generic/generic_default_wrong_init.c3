fn Type test_val(Type val = Type{}) <Type> => val; // #error: 'Type' is not a generic type. Did you want an initializer but forgot () around the type? That is, you typed 'Type { ... }' but intended '(Type) { ... }'

fn int main()
{
    int x = test_val{int}(1);
    int y = test_val{int}();
    return 0;
}
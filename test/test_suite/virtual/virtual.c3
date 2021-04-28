module foo;

extern func void test_virtual3(virtual* y);

interface Baz
{
    func void open();
    func int close();
}

extern func void test_virtual2(virtual Baz* z);

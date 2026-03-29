faultdef BAD_READ, OUT_OF_MEMORY;
fn void main()
{
    const char[][] URLS = { "", "", "", OUT_OF_MEMORY~, }; // #error: It is not possible to cast from 'char[][]?' to 'char[][]'
}

<*
 @require $defined(list[0]) : "list needs to be indexable"
 @require $defined(lengthof(list)) : "list needs to have a length"

 @require $defined(@body(list[0])) : "element needs to be assignable from list"
*>
macro void @vbox(list; @body(element))
{
    foreach(element : list)
    {
        @body(element);
    }
}

fn int main()
{
    int[] list = {10,34,5};
    @vbox(list; int element)
    {

    };
    @vbox(list; String element) {}; // #error: "$defined(@body(list[0]))" violated: 'element needs to be assignable from list'
    return 0;
}
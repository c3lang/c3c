module test;
import std::io;

fn void main()
{
	/*
        Here's a comment.
    	/*
    	    And a nested comment.
    	*/
    */
	char[] text = `
 		function hello() {
 		  console.log("name`"\t"`age");
 		}

 		hello();
 	`;
	io::println(text);

    // Binary
    const DATA = x"4749463839610100010080"
                 x"0100ffffff00000021f904"
                 x"010a0001002c0000000001"
                 x"0001000002024c01003b";
	io::printf("%d\n", DATA.len);
}
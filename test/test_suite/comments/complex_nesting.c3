// this test stress-tests the compiler ability to handle complex comments

/* first of all, I wanna say that <* linux *> is the best OS ever!
   secondly, #!pizza, it's a masterpiece... nothing is better than pizza, // thanks to who cooked it for the first time..
   I want to rant windows but I'm afraid of it getting mad and not supporting C3.. /* I'll not cause problems, just for this time */
   I'm forced to use windows for ~4 more months just for my school.. /* this is TERRIBLE guys */
   #! this line would fail because it's "#!" isn't in the right place (the first two characters of a file).. but since we're already in a comment, it' fine!
*/

#! this line should raise an error! // #error: a global
// while this shouldn't!

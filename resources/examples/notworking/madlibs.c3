module madlibs;
import regex, stdio;

fn void main()
{
    printn("Enter a story template, terminated by an empty line:");
    String story = "";
    while (try String line = stdin().readline().strip())
    {

        story = story.append(line);
        story = story.append("\n");
    }

    Regex r;

    r.initWithOptions("<.+?>", RegexOpt.GLOBAL)!!;
    defer r.destroy();

    foreach (RegexMatch* match : r.match(story))
    {
        VarString s = match.string;
        printf("Enter a value for '%s': ", s[1..^2]);
        String word = stdin().readline().strip()!;
        story = story.replace(s, word);
    }

    printn("\nThe story becomes:\n%s\n", story);
}
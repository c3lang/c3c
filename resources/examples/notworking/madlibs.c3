module madlibs;
import regex, stdio;

fn void main()
{
    println("Enter a story template, terminated by an empty line:");
    String story = "";
    while (1)
    {
        String line = stdin.readln().strip() else break;
        story = story.append(line);
        story = story.append("\n");
    }

    Regex r;

    r.initWithOptions("<.+?>", RegexOpt.GLOBAL) else unreachable;
    defer r.destroy();

    foreach (RegexMatch* match : r.match(story))
    {
        String s = match.string;
        printf("Enter a value for '%s': ", s[1..^2]);
        string word = stdin.readln().strip() else return;
        story = story.replace(s, word);
    }

    println("\nThe story becomes:\n%s\n", story);
}
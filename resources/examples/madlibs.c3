module madlibs;
import regex, stdio;

func void main()
{
    println("Enter a story template, terminated by an empty line:");
    string@ story = "";
    while (1)
    {
        try string line = stdin.readln().strip();
        if (!line.size) break;
        story += line + "\n";
    }

    Regex* r = try regex::regexWithOptions("<.+?>", RegexOpt.GLOBAL) catch @unreachable;

    foreach (RegexMatch* match : r.match(story))
    {
        string s = match.string;
        printf("Enter a value for '%s': ", s.slice(1, s.size - 2));
        try string word = strin.readln().strip();
        story = story.replace(s, word);
    }

    println("\nThe story becomes:\n%s\n", story);

    catch (error e)
    {
        // Ignore any error
    }
}
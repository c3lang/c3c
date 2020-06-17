module madlibs;
import regex, stdio;

func void main()
{
    println("Enter a story template, terminated by an empty line:");
    string story = "";
    while (1)
    {
        string line = stdin.readln().strip() else "";
        if (!line.size) break;
        story += line + "\n";
    }

    Regex r;

    r.initWithOptions("<.+?>", RegexOpt.GLOBAL) else @unreachable;
    defer r.destroy();

    foreach (RegexMatch* match : r.match(story))
    {
        string s = match.string;
        printf("Enter a value for '%s': ", s.slice(1, s.size - 2));
        string! word = strin.readln().strip();
        catch (word)
        {
            // Exit on error.
            return;
        }
        story = story.replace(s, word);
    }

    println("\nThe story becomes:\n%s\n", story);
}
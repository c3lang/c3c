import curl;

fn int main(void)
{
    Curl curl;

    if (catch e = curl.init())
    {
        io::printfn("Failed to create new curl: %s\n", e);
        exit(FAILURE);
    }

    curl.setopt(URL, "http://www.rosettacode.org/");
    curl.setopt(FOLLOWLOCATION, 1);

    if (catch err = curl.perform())
    {
        io::printfn("Error making request: %s\n", e);
        exit(FAILURE);
    }

    exit(SUCCESS);

}

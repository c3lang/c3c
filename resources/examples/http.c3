import curl;

int main(void)
{
    try Curl curl = Curl.new();

    catch (error e)
    {
        printf("Failed to create new curl: %s\n", e.message);
        exit(FAILURE);
    }

    curl.setopt(URL, "http://www.rosettacode.org/");
    curl.setopt(FOLLOWLOCATION, 1);

    try curl.perform();

    catch (CurlError e)
    {
        printf("Error making request: %s\n", e.message);
        exit(FAILURE);
    }

    exit(SUCCESS);

}

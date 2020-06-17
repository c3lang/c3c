import curl;

func int main(void)
{
    Curl curl;

    catch (e = curl.init())
    {
        printf("Failed to create new curl: %s\n", e.message);
        exit(FAILURE);
    }

    curl.setopt(URL, "http://www.rosettacode.org/");
    curl.setopt(FOLLOWLOCATION, 1);

    catch (e = curl.perform())
    {
        printf("Error making request: %s\n", e.message);
        exit(FAILURE);
    }

    exit(SUCCESS);

}

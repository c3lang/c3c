// httplib3.c3
namespace httplib3;

// Import required modules
public import std.net;        // For networking functionalities
public import std.string;     // For string manipulations
public import std.error;      // For error handling

/**
 * HttpClient class provides basic HTTP functionalities
 * such as sending GET and POST requests.
 */
public class HttpClient {
    private var connection: TcpClient;

    /**
     * Constructor for the HttpClient class.
     * Initializes the TcpClient for connection handling.
     */
    public HttpClient() {
        this.connection = TcpClient();
    }

    /**
     * Sends an HTTP GET request to the specified URL.
     * 
     * @param url The URL to send the GET request to.
     * @return The response from the server as a string.
     */
    public fn get(url: string): string {
        var response = this.send_request("GET", url, "");
        return response;
    }

    /**
     * Sends an HTTP POST request to the specified URL with the given data.
     * 
     * @param url The URL to send the POST request to.
     * @param data The data to be sent in the POST request body.
     * @return The response from the server as a string.
     */
    public fn post(url: string, data: string): string {
        var response = this.send_request("POST", url, data);
        return response;
    }

    /**
     * Sends an HTTP request to the server.
     * 
     * @param method The HTTP method to use (e.g., GET, POST).
     * @param url The URL to send the request to.
     * @param data The data to send with the request (for POST).
     * @return The response from the server as a string.
     */
    private fn send_request(method: string, url: string, data: string): string {
        var host = this.extract_host(url);
        var path = this.extract_path(url);

        // Connect to the server on port 80 (default for HTTP)
        this.connection.connect(host, 80);

        // Construct the HTTP request string
        var request = method + " " + path + " HTTP/1.1\r\n";
        request += "Host: " + host + "\r\n";
        request += "Connection: close\r\n";
        if (method == "POST") {
            request += "Content-Length: " + string.len(data) + "\r\n";
            request += "Content-Type: application/x-www-form-urlencoded\r\n";
        }
        request += "\r\n";
        if (method == "POST") {
            request += data;
        }

        // Send the HTTP request
        this.connection.write(request);

        // Read the response from the server
        var response = this.connection.read();

        // Close the connection
        this.connection.close();

        return response;
    }

    /**
     * Extracts the host from the given URL.
     * 
     * @param url The URL to extract the host from.
     * @return The extracted host as a string.
     */
    private fn extract_host(url: string): string {
        // Simple host extraction logic assuming URL format: http://host/path
        var start = string.find(url, "//") + 2;
        var end = string.find(url, "/", start);
        if (end == -1) {
            end = string.len(url);
        }
        return string.slice(url, start, end);
    }

    /**
     * Extracts the path from the given URL.
     * 
     * @param url The URL to extract the path from.
     * @return The extracted path as a string.
     */
    private fn extract_path(url: string): string {
        // Simple path extraction logic assuming URL format: http://host/path
        var start = string.find(url, "//") + 2;
        var path_start = string.find(url, "/", start);
        if (path_start == -1) {
            return "/";
        }
        return string.slice(url, path_start, string.len(url));
    }
}

module urltest @test;

import std::io;
import std::net::url;

// Parser tests

fn void test_parse_foo()
{
	Url url = url::new_parse("foo://example.com:8042/over/there?name=ferret#nose")!!;
	defer url.free();

	assert(url.scheme == "foo", "got '%s'", url.scheme);
	assert(url.host == "example.com", "got '%s'", url.host);
	assert(url.port == 8042, "got '%d'", url.port);
	assert(url.username == "", "got '%s'", url.username);
	assert(url.password == "", "got '%s'", url.password);
	assert(url.path == "/over/there", "got '%s'", url.path);
	assert(url.query == "name=ferret", "got '%s'", url.query);
	assert(url.fragment == "nose", "got: '%s'", url.fragment);
}

fn void test_parse_urn()
{
	Url url = url::new_parse("urn:example:animal:ferret:nose")!!;
	defer url.free();

	assert(url.scheme == "urn");
	assert(url.host == "");
	assert(url.port == 0);
	assert(url.username == "", "got '%s'", url.username);
	assert(url.password == "", "got '%s'", url.password);
	assert(url.path == "example:animal:ferret:nose");
	assert(url.query == "");
	assert(url.fragment == "");
}

fn void test_parse_jdbc()
{
	Url url = url::new_parse("jdbc:mysql://test_user:ouupppssss@localhost:3306/sakila?profileSQL=true")!!;
	defer url.free();

	assert(url.scheme == "jdbc:mysql");
	assert(url.host == "localhost");
	assert(url.port == 3306);
	assert(url.username == "test_user", "got '%s'", url.username);
	assert(url.password == "ouupppssss", "got '%s'", url.password);
	assert(url.path == "/sakila");
	assert(url.query == "profileSQL=true");
	assert(url.fragment == "");
}

fn void test_parse_ftp()
{
	Url url = url::new_parse("ftp://ftp.is.co.za/rfc/rfc1808.txt")!!;
	defer url.free();

	assert(url.scheme == "ftp");
	assert(url.host == "ftp.is.co.za");
	assert(url.port == 0);
	assert(url.username == "", "got '%s'", url.username);
	assert(url.password == "", "got '%s'", url.password);
	assert(url.path == "/rfc/rfc1808.txt");
	assert(url.query == "");
	assert(url.fragment == "");
}

fn void test_parse_http()
{
	Url url = url::new_parse("http://www.ietf.org/rfc/rfc2396.txt#header1")!!;
	defer url.free();

	assert(url.scheme == "http");
	assert(url.host == "www.ietf.org");
	assert(url.port == 0);
	assert(url.username == "", "got '%s'", url.username);
	assert(url.password == "", "got '%s'", url.password);
	assert(url.path == "/rfc/rfc2396.txt");
	assert(url.query == "");
	assert(url.fragment == "header1");
}

fn void test_parse_ldap()
{
	Url url = url::new_parse("ldap://[2001:db8::7]/c=GB?objectClass=one&objectClass=two")!!;
	defer url.free();

	assert(url.scheme == "ldap");
	assert(url.host == "[2001:db8::7]");
	assert(url.port == 0);
	assert(url.username == "", "got '%s'", url.username);
	assert(url.password == "", "got '%s'", url.password);
	assert(url.path == "/c=GB");
	assert(url.query == "objectClass=one&objectClass=two");
	assert(url.fragment == "");
}

fn void test_parse_mailto()
{
	Url url = url::new_parse("mailto:John.Doe@example.com")!!;
	defer url.free();

	assert(url.scheme == "mailto");
	assert(url.host == "");
	assert(url.port == 0);
	assert(url.username == "", "got '%s'", url.username);
	assert(url.password == "", "got '%s'", url.password);
	assert(url.path == "John.Doe@example.com");
	assert(url.query == "");
	assert(url.fragment == "");
}

fn void test_new_parses()
{
	Url url = url::new_parse("news:comp.infosystems.www.servers.unix")!!;
	defer url.free();

	assert(url.scheme == "news");
	assert(url.host == "");
	assert(url.port == 0);
	assert(url.username == "", "got '%s'", url.username);
	assert(url.password == "", "got '%s'", url.password);
	assert(url.path == "comp.infosystems.www.servers.unix");
	assert(url.query == "");
	assert(url.fragment == "");
}

fn void test_parse_tel()
{
	Url url = url::new_parse("tel:+1-816-555-1212")!!;
	defer url.free();

	assert(url.scheme == "tel");
	assert(url.host == "");
	assert(url.port == 0);
	assert(url.username == "", "got '%s'", url.username);
	assert(url.password == "", "got '%s'", url.password);
	assert(url.path == "+1-816-555-1212");
	assert(url.query == "");
	assert(url.fragment == "");
}

fn void test_parse_telnet()
{
	Url url = url::new_parse("telnet://192.0.2.16:80/")!!;
	defer url.free();

	assert(url.scheme == "telnet");
	assert(url.host == "192.0.2.16");
	assert(url.port == 80);
	assert(url.username == "", "got '%s'", url.username);
	assert(url.password == "", "got '%s'", url.password);
	assert(url.path == "/");
	assert(url.query == "");
	assert(url.fragment == "");
}

fn void test_parse_urn2()
{
	Url url = url::new_parse("urn:oasis:names:specification:docbook:dtd:xml:4.1.2")!!;
	defer url.free();

	assert(url.scheme == "urn");
	assert(url.host == "");
	assert(url.port == 0);
	assert(url.username == "", "got '%s'", url.username);
	assert(url.password == "", "got '%s'", url.password);
	assert(url.path == "oasis:names:specification:docbook:dtd:xml:4.1.2");
	assert(url.query == "");
	assert(url.fragment == "");
}

fn void test_parse_empty()
{
	assert(@catch(url::new_parse("       ")) == UrlParsingResult.EMPTY);
}

// Parser tests with escape sequences

fn void test_parse_path_with_escape_sequence()
{
	Url url = url::new_parse("foo://example.com:8042/file/name%20one%26two?name=ferret#nose")!!;
	defer url.free();

	assert(url.scheme == "foo", "got '%s'", url.scheme);
	assert(url.host == "example.com", "got '%s'", url.host);
	assert(url.port == 8042, "got '%d'", url.port);
	assert(url.username == "", "got '%s'", url.username);
	assert(url.password == "", "got '%s'", url.password);
	assert(url.path == "/file/name one&two", "got '%s'", url.path);
	assert(url.query == "name=ferret", "got '%s'", url.query);
	assert(url.fragment == "nose", "got: '%s'", url.fragment);
}

fn void test_parse_username_and_password_with_escape_sequence()
{
	Url url = url::new_parse("jdbc:mysql://test%20user:ouu%40pppssss@localhost:3306/sakila?profileSQL=true")!!;
	defer url.free();

	assert(url.scheme == "jdbc:mysql");
	assert(url.host == "localhost");
	assert(url.port == 3306);
	assert(url.username == "test user", "got '%s'", url.username);
	assert(url.password == "ouu@pppssss", "got '%s'", url.password);
	assert(url.path == "/sakila");
	assert(url.query == "profileSQL=true");
	assert(url.fragment == "");
}

fn void test_parse_fragment_with_escape_sequence()
{
	Url url = url::new_parse("http://www.ietf.org/rfc/rfc2396.txt#header%201%262")!!;
	defer url.free();

	assert(url.scheme == "http");
	assert(url.host == "www.ietf.org");
	assert(url.port == 0);
	assert(url.username == "", "got '%s'", url.username);
	assert(url.password == "", "got '%s'", url.password);
	assert(url.path == "/rfc/rfc2396.txt");
	assert(url.query == "");
	assert(url.fragment == "header 1&2");
}

// to_string() tests

fn void test_string_foo()
{
	Url url = {.scheme="foo", .host="example.com", .port=8042, .path="/over/there", .query="name=ferret", .fragment="nose"};
	String str = string::new_format("%s", url);
	defer str.free();

	assert(str == "foo://example.com:8042/over/there?name=ferret#nose");
}

fn void test_string_urn()
{
	Url url = {.scheme="urn", .path="example:animal:ferret:nose"};
	String str = string::new_format("%s", url);
	defer str.free();

	assert(str == "urn:example:animal:ferret:nose");
}

fn void test_string_jdbc()
{
	Url url = {.scheme="jdbc:mysql", .host="localhost", .port=3306, .username="test_user", .password="ouupppssss", .path="/sakila", .query="profileSQL=true"};
	String str = string::new_format("%s", url);
	defer str.free();

	assert(str == "jdbc:mysql://test_user:ouupppssss@localhost:3306/sakila?profileSQL=true");
}

fn void test_string_ftp()
{
	Url url = {.scheme="ftp", .host="ftp.is.co.za", .path="/rfc/rfc1808.txt"};
	String str = string::new_format("%s", url);
	defer str.free();

	assert(str == "ftp://ftp.is.co.za/rfc/rfc1808.txt");
}

fn void test_string_http()
{
	Url url = {.scheme="http", .host="www.ietf.org", .path="/rfc/rfc2396.txt", .fragment="header1"};
	String str = string::new_format("%s", url);
	defer str.free();

	assert(str == "http://www.ietf.org/rfc/rfc2396.txt#header1", "got: '%s'", str);
}

fn void test_string_ldap()
{
	Url url = {.scheme="ldap", .host="[2001:db8::7]", .path="/c=GB", .query="objectClass=one&objectClass=two"};
	String str = string::new_format("%s", url);
	defer str.free();

	assert(str == "ldap://[2001:db8::7]/c=GB?objectClass=one&objectClass=two", "got: '%s'", str);
}

fn void test_string_mailto()
{
	Url url = {.scheme="mailto", .path="John.Doe@example.com"};
	String str = string::new_format("%s", url);
	defer str.free();

	assert(str == "mailto:John.Doe@example.com");
}

fn void test_string_news()
{
	Url url = {.scheme="news", .path="comp.infosystems.www.servers.unix"};
	String str = string::new_format("%s", url);
	defer str.free();
	assert(str == "news:comp.infosystems.www.servers.unix");
}

fn void test_string_tel()
{
	Url url = {.scheme="tel", .path="+1-816-555-1212"};
	String str = string::new_format("%s", url);
	defer str.free();

	assert(str == "tel:+1-816-555-1212");
}

fn void test_string_telnet()
{
	Url url = {.scheme="telnet", .host="192.0.2.16", .port=80, .path="/"};
	String str = string::new_format("%s", url);
	defer str.free();

	assert(str == "telnet://192.0.2.16:80/");
}

fn void test_string_urn2()
{
	Url url = {.scheme="urn", .path="oasis:names:specification:docbook:dtd:xml:4.1.2"};
	String str = string::new_format("%s", url);
	defer str.free();

	assert(str == "urn:oasis:names:specification:docbook:dtd:xml:4.1.2");
}

fn void test_string_empty()
{
	Url url = {};
	String str = string::new_format("%s", url);
	defer str.free();

	assert(str == "");
}

// query_values

fn void test_query_values1()
{
	Url url = url::new_parse("foo://example.com:8042/over/there?name=ferret=ok#nose")!!;
	defer url.free();

	UrlQueryValues vals = url::temp_parse_query(url.query);
	defer vals.free();

	assert(vals.len() == 1);
	UrlQueryValueList l = vals["name"]!!;

	assert(l.len() == 1);
	assert(l[0] == "ferret=ok");
}

fn void test_query_values2()
{
	Url url = url::new_parse("foo://example.com:8042/over/there?name=ferret&age=99&age=11#nose")!!;
	defer url.free();

	UrlQueryValues vals = url::new_parse_query(url.query);
	defer vals.free();
	assert(vals.len() == 2);

	UrlQueryValueList l_name = vals["name"]!!;
	assert(l_name.len() == 1);
	assert(l_name[0] == "ferret");

	UrlQueryValueList l_age = vals["age"]!!;
	assert(l_age.len() == 2);
	assert(l_age[0] == "99");
	assert(l_age[1] == "11");
}

fn void test_escaped_query_values()
{
	Url url = url::new_parse("foo://example.com:8042/over/there?k%3Bey=%3Ckey%3A+0x90%3E&age=99&age=11#nose")!!;
	defer url.free();

	UrlQueryValues vals = url::new_parse_query(url.query);
	defer vals.free();
	assert(vals.len() == 2);

	UrlQueryValueList l_key = vals["k;ey"]!!;
	assert(l_key.len() == 1);
	assert(l_key[0] == "<key: 0x90>");
}

fn void test_query_values_withempty()
{
	Url url = url::new_parse("foo://example.com:8042/over/there?name=ferret&&&age=99&age=11")!!;
	defer url.free();

	UrlQueryValues vals = url::new_parse_query(url.query);
	defer vals.free();
	assert(vals.len() == 2);
}

// url compose and parse should be idempotent

fn void test_url_idempotence()
{
	UrlQueryValues query_builder;
	query_builder.new_init();
	defer query_builder.free();

	query_builder.add("profileSQL", "true");
	query_builder.add("k;ey", "<key: 0x90>");

	String query = query_builder.to_string();
	defer query.free();

	Url url = {
		.scheme    = "jdbc:mysql",
		.host      = "localhost",
		.port      = 3306,
		.username  = "test user",
		.password  = "ouu@pppssss",
		.path      = "/sakila",
		.query = query,
		.fragment  = "no se",
	};

	String url_string = url.to_string();
	defer url_string.free();

	String want = "jdbc:mysql://test%20user:ouu%40pppssss@localhost:3306"
		"/sakila?profileSQL=true&k%3Bey=%3Ckey%3A+0x90%3E#no%20se";
	assert(url_string == want, "got: %s, want: %s", url_string, want);

	Url parsed = url::new_parse(url_string)!!;
	defer parsed.free();

	UrlQueryValues vals = url::new_parse_query(parsed.query);
	defer vals.free();
	assert(vals.len() == 2);

	UrlQueryValueList key;
	key = vals["k;ey"]!!;
	assert(key.len() == 1);
	assert(key[0] == "<key: 0x90>");

	key = vals["profileSQL"]!!;
	assert(key.len() == 1);
	assert(key[0] == "true");

	String parsed_query = vals.to_string();
	defer parsed_query.free();

	assert(parsed.scheme == url.scheme);
	assert(parsed.host == url.host);
	assert(parsed.port == url.port);
	assert(parsed.username == url.username);
	assert(parsed.password == url.password);
	assert(parsed.path == url.path);
	assert(parsed.query == parsed_query);
	assert(parsed.fragment == url.fragment);

	String parsed_string = parsed.to_string();
	defer parsed_string.free();

	assert(url_string == parsed_string);
}


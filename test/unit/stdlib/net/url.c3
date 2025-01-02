module urltest @test;

import std::io;
import std::net::url;

// Parser tests

fn void! test_parse_foo()
{
	Url url = url::parse("foo://example.com:8042/over/there?name=ferret#nose")!;

	assert(url.scheme == "foo", "got '%s'", url.scheme);
	assert(url.host == "example.com", "got '%s'", url.host);
	assert(url.port == 8042, "got '%d'", url.port);
	assert(url.username == "", "got '%s'", url.username);
	assert(url.password == "", "got '%s'", url.password);
	assert(url.path == "/over/there", "got '%s'", url.path);
	assert(url.query == "name=ferret", "got '%s'", url.query);
	assert(url.fragment == "nose", "got: '%s'", url.fragment);
}

fn void! test_parse_urn()
{
	Url url = url::parse("urn:example:animal:ferret:nose")!;

	assert(url.scheme == "urn");
	assert(url.host == "");
	assert(url.port == 0);
	assert(url.username == "", "got '%s'", url.username);
	assert(url.password == "", "got '%s'", url.password);
	assert(url.path == "example:animal:ferret:nose");
	assert(url.query == "");
	assert(url.fragment == "");
}

fn void! test_parse_jdbc()
{
	Url url = url::parse("jdbc:mysql://test_user:ouupppssss@localhost:3306/sakila?profileSQL=true")!;

	assert(url.scheme == "jdbc:mysql");
	assert(url.host == "localhost");
	assert(url.port == 3306);
	assert(url.username == "test_user", "got '%s'", url.username);
	assert(url.password == "ouupppssss", "got '%s'", url.password);
	assert(url.path == "/sakila");
	assert(url.query == "profileSQL=true");
	assert(url.fragment == "");
}

fn void! test_parse_ftp()
{
	Url url = url::parse("ftp://ftp.is.co.za/rfc/rfc1808.txt")!;

	assert(url.scheme == "ftp");
	assert(url.host == "ftp.is.co.za");
	assert(url.port == 0);
	assert(url.username == "", "got '%s'", url.username);
	assert(url.password == "", "got '%s'", url.password);
	assert(url.path == "/rfc/rfc1808.txt");
	assert(url.query == "");
	assert(url.fragment == "");
}

fn void! test_parse_http()
{
	Url url = url::parse("http://www.ietf.org/rfc/rfc2396.txt#header1")!;

	assert(url.scheme == "http");
	assert(url.host == "www.ietf.org");
	assert(url.port == 0);
	assert(url.username == "", "got '%s'", url.username);
	assert(url.password == "", "got '%s'", url.password);
	assert(url.path == "/rfc/rfc2396.txt");
	assert(url.query == "");
	assert(url.fragment == "header1");
}

fn void! test_parse_ldap()
{
	Url url = url::parse("ldap://[2001:db8::7]/c=GB?objectClass=one&objectClass=two")!;

	assert(url.scheme == "ldap");
	assert(url.host == "[2001:db8::7]");
	assert(url.port == 0);
	assert(url.username == "", "got '%s'", url.username);
	assert(url.password == "", "got '%s'", url.password);
	assert(url.path == "/c=GB");
	assert(url.query == "objectClass=one&objectClass=two");
	assert(url.fragment == "");
}

fn void! test_parse_mailto()
{
	Url url = url::parse("mailto:John.Doe@example.com")!;

	assert(url.scheme == "mailto");
	assert(url.host == "");
	assert(url.port == 0);
	assert(url.username == "", "got '%s'", url.username);
	assert(url.password == "", "got '%s'", url.password);
	assert(url.path == "John.Doe@example.com");
	assert(url.query == "");
	assert(url.fragment == "");
}

fn void! test_parse_news()
{
	Url url = url::parse("news:comp.infosystems.www.servers.unix")!;

	assert(url.scheme == "news");
	assert(url.host == "");
	assert(url.port == 0);
	assert(url.username == "", "got '%s'", url.username);
	assert(url.password == "", "got '%s'", url.password);
	assert(url.path == "comp.infosystems.www.servers.unix");
	assert(url.query == "");
	assert(url.fragment == "");
}

fn void! test_parse_tel()
{
	Url url = url::parse("tel:+1-816-555-1212")!;

	assert(url.scheme == "tel");
	assert(url.host == "");
	assert(url.port == 0);
	assert(url.username == "", "got '%s'", url.username);
	assert(url.password == "", "got '%s'", url.password);
	assert(url.path == "+1-816-555-1212");
	assert(url.query == "");
	assert(url.fragment == "");
}

fn void! test_parse_telnet()
{
	Url url = url::parse("telnet://192.0.2.16:80/")!;

	assert(url.scheme == "telnet");
	assert(url.host == "192.0.2.16");
	assert(url.port == 80);
	assert(url.username == "", "got '%s'", url.username);
	assert(url.password == "", "got '%s'", url.password);
	assert(url.path == "/");
	assert(url.query == "");
	assert(url.fragment == "");
}

fn void! test_parse_urn2()
{
	Url url = url::parse("urn:oasis:names:specification:docbook:dtd:xml:4.1.2")!;

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
	Url url = url::parse("       ")!;

	assert(url.scheme == "");
	assert(url.host == "");
	assert(url.port == 0);
	assert(url.username == "", "got '%s'", url.username);
	assert(url.password == "", "got '%s'", url.password);
	assert(url.path == "");
	assert(url.query == "");
	assert(url.fragment == "");
}

// to_string() tests

fn void! test_string_foo()
{
	Url url = {.scheme="foo", .host="example.com", .port=8042, .path="/over/there", .query="name=ferret", .fragment="nose"};
	String str = string::new_format("%s", url);

	assert(str == "foo://example.com:8042/over/there?name=ferret#nose");
}

fn void! test_string_urn()
{
	Url url = {.scheme="urn", .path="example:animal:ferret:nose"};
	String str = string::new_format("%s", url);

	assert(str == "urn:example:animal:ferret:nose");
}

fn void! test_string_jdbc()
{
	Url url = {.scheme="jdbc:mysql", .host="localhost", .port=3306, .username="test_user", .password="ouupppssss", .path="/sakila", .query="profileSQL=true"};
	String str = string::new_format("%s", url);

	assert(str == "jdbc:mysql://test_user:ouupppssss@localhost:3306/sakila?profileSQL=true");
}

fn void! test_string_ftp()
{
	Url url = {.scheme="ftp", .host="ftp.is.co.za", .path="/rfc/rfc1808.txt"};
	String str = string::new_format("%s", url);

	assert(str == "ftp://ftp.is.co.za/rfc/rfc1808.txt");
}

fn void! test_string_http()
{
	Url url = {.scheme="http", .host="www.ietf.org", .path="/rfc/rfc2396.txt#header1"};
	String str = string::new_format("%s", url);

	assert(str == "http://www.ietf.org/rfc/rfc2396.txt#header1");
}

fn void! test_string_ldap()
{
	Url url = {.scheme="ldap", .host="[2001:db8::7]", .path="/c=GB?objectClass=one&objectClass=two"};
	String str = string::new_format("%s", url);

	assert(str == "ldap://[2001:db8::7]/c=GB?objectClass=one&objectClass=two");
}

fn void! test_string_mailto()
{
	Url url = {.scheme="mailto", .path="John.Doe@example.com"};
	String str = string::new_format("%s", url);

	assert(str == "mailto:John.Doe@example.com");
}

fn void! test_string_news()
{
	Url url = {.scheme="news", .path="comp.infosystems.www.servers.unix"};
	String str = string::new_format("%s", url);
	assert(str == "news:comp.infosystems.www.servers.unix");
}

fn void! test_string_tel()
{
	Url url = {.scheme="tel", .path="+1-816-555-1212"};
	String str = string::new_format("%s", url);

	assert(str == "tel:+1-816-555-1212");
}

fn void! test_string_telnet()
{
	Url url = {.scheme="telnet", .host="192.0.2.16", .port=80, .path="/"};
	String str = string::new_format("%s", url);

	assert(str == "telnet://192.0.2.16:80/");
}

fn void! test_string_urn2()
{
	Url url = {.scheme="urn", .path="oasis:names:specification:docbook:dtd:xml:4.1.2"};
	String str = string::new_format("%s", url);

	assert(str == "urn:oasis:names:specification:docbook:dtd:xml:4.1.2");
}

fn void! test_string_empty()
{
	Url url = {};
	String str = string::new_format("%s", url);

	assert(str == "");
}

// query_values

fn void! test_query_values1()
{
	Url url = url::parse("foo://example.com:8042/over/there?name=ferret=ok#nose")!;

	UrlQueryValues vals = url.new_query_values();
	defer vals.free();

	assert(vals.len() == 1);
	UrlQueryValueList l = vals["name"]!;

	assert(l.len() == 1);
	assert(l[0] == "ferret=ok");
}

fn void! test_query_values2()
{
	Url url = url::parse("foo://example.com:8042/over/there?name=ferret&age=99&age=11#nose")!;

	UrlQueryValues vals = url.new_query_values();
	defer vals.free();
	assert(vals.len() == 2);

	UrlQueryValueList l_name = vals["name"]!;
	assert(l_name.len() == 1);
	assert(l_name[0] == "ferret");

	UrlQueryValueList l_age = vals["age"]!;
	assert(l_age.len() == 2);
	assert(l_age[0] == "99");
	assert(l_age[1] == "11");
}

fn void! test_query_values_withempty()
{
	Url url = url::parse("foo://example.com:8042/over/there?name=ferret&&&age=99&age=11")!;

	UrlQueryValues vals = url.new_query_values();
	defer vals.free();
	assert(vals.len() == 2);
}
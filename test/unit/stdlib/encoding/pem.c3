// Copyright (c) 2026 Zack Puhl <github@xmit.xyz>. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module pem_test @test;

import std::collections::pair, std::encoding::pem, std::io;


fn void decode_single()
{
	Pem[] res = pem::decode(tmem, SINGLE)!!;
	test::@check(res.len == 1);
	test::@check(res[0].tag == "CERTIFICATE");
	test::@check(res[0].headers.len() == 0);
	test::@check(res[0].data == SINGLE_DATA);
}

fn void encode_single()
{
	test::@check(pem::encode(tmem, SINGLE_DATA, "CERTIFICATE")!![..^2] == SINGLE);
	test::@check(pem::encode(tmem, SINGLE_DATA, "CERTIFICATE", use_crlf: true)!![..^3] == SINGLE.treplace("\n", "\r\n"));
}

fn void decode_multi()
{
	const Pair{String, String}[] EXPECTED = {
		{ "Something", "Here" },
		{ "A-Multiline", "Header Goes in this spot And another line to back it up" },
		{ "Non-Multi", "line header!" },
		{ "I'm-trying", "to confuse things: when parsing the content of this file" },
		{ "done", "now" },
	};
	Pem[] res = pem::decode(tmem, MULTI)!!;
	test::@check(res.len == 2);
	test::@check(res[0].tag == "THIS IS NOT A REAL PRIVATE KEY");
	test::@check(res[0].data == MULTI_DATA);
	test::@check(res[0].headers.len() == 5);
	foreach (pair : EXPECTED)
	{
		String val = res[0].headers[pair.first]!!;
		test::@check(res[0].headers.has_key(pair.first), "Expected header key '%s' but it wasn't found.", pair.first);
		test::@check(val == pair.second, "Expected value '%s' for key '%s' but did not get a match (got: '%s').", pair.second, pair.first, val);
	}
	test::@check(res[1].tag == "CERTIFICATE");
	test::@check(res[1].headers.len() == 0);
	test::@check(res[1].data == SINGLE_DATA);
}

fn void encode_multi()
{
	String result = pem::encode_many(tmem, { SINGLE_DATA, MULTI_DATA }, { "CERTIFICATE", "THING" })!!;
	test::@check(result == SIMPLE_MULTI, "Got:\n%s\n\nExpected:\n%s\n\n", result, SIMPLE_MULTI);
}

fn void encode_single_with_headers()
{
	String result = pem::encode(tmem, SINGLE_DATA, "CERTIFICATE", {"My-Header", "Example"}, {"This-is-a-longer-header", "with some extra content that will bleed over to the next line."}, {"Last", "Header is single-line."})!!;
	test::@check(result == SINGLE_WITH_HEADERS, "Got:\n%s\n\nExpected:\n%s\n\n", result, SINGLE_WITH_HEADERS);
}

fn void leak_check_decode()
{
	Pem[] res = pem::decode(mem, MULTI)!!;
	test::@check(res.len == 2);

	// don't optimize it away >:I
	io::printfn(res[0].tag);
	io::printfn(res[1].tag);

	foreach (r : res) r.free();
	mem::free(res);

	// ==========================================
	res = pem::decode(mem, SINGLE)!!;
	test::@check(res.len == 1);
	io::printfn(res[0].tag);

	foreach (r : res) r.free();
	mem::free(res);
}

fn void leak_check_encode()
{
	String encoded = pem::encode(mem, SINGLE_DATA, "SOMETHING", { "Key", "Value" }, {"KeyX", "ValueX" })!!;
	test::@check(encoded.len > 0);
	io::printfn("ENCODED_SINGLE:\n%s\n", encoded);
	encoded.free(mem);

	encoded = pem::encode_many(mem, { SINGLE_DATA, MULTI_DATA }, { "CERTIFICATE", "THING" }, (String[2][]) { {"Key1", "Value1"}, {"KeyX", "ValueX"} }, (String[2][]){ {"more", "more"} })!!;
	test::@check(encoded.len > 0);
	io::printfn("ENCODED_MULTI:\n%s\n", encoded);
	encoded.free(mem);
}

fn void decode_errors()
{
	foreach (i, pair : DECODE_ERRORS) test::@error(pem::decode(tmem, pair.first), pair.second);
}


// =============================================================================================
// These PEM blobs aren't 'real' in the sense that they're completely public. Tbh I kinda swiped
//   them from another public repository, and the underlying CN is Superfish, Inc. If these were
//   real exfiltrations at some point in time, then they're widely public as is... No harm done.

const String SINGLE = `-----BEGIN CERTIFICATE-----
MIIC9TCCAl6gAwIBAgIJANL8E4epRNznMA0GCSqGSIb3DQEBBQUAMFsxGDAWBgNV
BAoTD1N1cGVyZmlzaCwgSW5jLjELMAkGA1UEBxMCU0YxCzAJBgNVBAgTAkNBMQsw
CQYDVQQGEwJVUzEYMBYGA1UEAxMPU3VwZXJmaXNoLCBJbmMuMB4XDTE0MDUxMjE2
MjUyNloXDTM0MDUwNzE2MjUyNlowWzEYMBYGA1UEChMPU3VwZXJmaXNoLCBJbmMu
MQswCQYDVQQHEwJTRjELMAkGA1UECBMCQ0ExCzAJBgNVBAYTAlVTMRgwFgYDVQQD
Ew9TdXBlcmZpc2gsIEluYy4wgZ8wDQYJKoZIhvcNAQEBBQADgY0AMIGJAoGBAOjz
Shh2Xxk/sc9Y6X9DBwmVgDXFD/5xMSeBmRImIKXfj2r8QlU57gk4idngNsSsAYJb
1Tnm+Y8HiN/+7vahFM6pdEXY/fAXVyqC4XouEpNarIrXFWPRt5tVgA9YvBxJ7SBi
3bZMpTrrHD2g/3pxptMQeDOuS8Ic/ZJKocPnQaQtAgMBAAGjgcAwgb0wDAYDVR0T
BAUwAwEB/zAdBgNVHQ4EFgQU+5izU38URC7o7tUJml4OVoaoNYgwgY0GA1UdIwSB
hTCBgoAU+5izU38URC7o7tUJml4OVoaoNYihX6RdMFsxGDAWBgNVBAoTD1N1cGVy
ZmlzaCwgSW5jLjELMAkGA1UEBxMCU0YxCzAJBgNVBAgTAkNBMQswCQYDVQQGEwJV
UzEYMBYGA1UEAxMPU3VwZXJmaXNoLCBJbmMuggkA0vwTh6lE3OcwDQYJKoZIhvcN
AQEFBQADgYEApHyg7ApKx3DEcWjzOyLi3JyN0JL+c35yK1VEmxu0Qusfr76645Oj
1IsYwpTws6a9ZTRMzST4GQvFFQra81eLqYbPbMPuhC+FCxkUF5i0DNSWi+kczJXJ
TtCqSwGl9t9JEoFqvtW+znZ9TqyLiOMw7TGEUI+88VAqW0qmXnwPcfo=
-----END CERTIFICATE-----`;

const String MULTI =
`-----BEGIN THIS IS NOT A REAL PRIVATE KEY-----
Something: Here
A-Multiline: Header
 Goes in this spot
 And another line to back it up
Non-Multi: line header!
I'm-trying: to confuse things: when parsing
	     	   	the content of this file
done: now

MIICxjBABgkqhkiG9w0BBQ0wMzAbBgkqhkiG9w0BBQwwDgQIDHHhyAEZQoICAggA
MBQGCCqGSIb3DQMHBAiHEg+MCYQ30ASCAoDEvGvFRHvtWOb5Rc0f3lbVKqeUvWSz
xQn+rZELHnwb6baolmbFcsi6XkacVzL/EF7Ll4de/CSQ6pZZCCvfDzov0mPOuGve
SAe7hbAcol7+JWVfzbnVTblPf0i7mwSvK61cKq7YfcKJ2os/uJGpeX9zraywWyFx
f+EdTr348dOez8uHkURyY1cvSHsIdITALkChOonAYT68SVighTeB6xOCwfmsHx+X
3Qbhom2YCIxfJiaAoz2/LndCpDaEfOrVrxXFOKXrIbmeDEyjDQj16AVni9uuaj7l
NiO3zrrqxsfdVINPaAYRKQnS102jXqkH01z72c/MpMMC6dwZswF5V3R7RSXngyBn
1GLxVFHKR753Gt0IDag13Bd8Jt890/v0tE0Kx66jCkRGn+VCq6+bsnh7VpTH/cG5
dlFnv56lv2leknu5ghdJHX8YQ6HjnioaaheLA+ORAxqAlD8Itt1/pRBOOMSkutdz
d1px9dB2ZBpSoRAOcBwU5aFaw9uu+tXyzrPM3tZomu8ryQYMNlmVgPNDJOz6jPJi
jaZHWTS7U6j370oH/B0KTUG/ybrJGFnOmPP4h2u/ugG75EkfotURsvbrWuetQhOi
TCH+9nbIcT3pxnTXqI2IRHZXMturQ+6fqlJF3bb9bWarMBuC3KgprqyqXxeM0Sqg
VlyKLWwAuMf2Ec7t7ujqaNmVgv6bpwHEbR6njIi7lC7j4w6D2YQ8vacgvS3MB/K0
SX54HNVBVuXhAixPtYJ6tOBGm7QFAKaXju0PJ+AljnMEsHRekOs2u42OHBXEWDE8
VHw7/lTXWsJkBcQM+g/svyqV4xKHDAixPms2SUwJyKjvEgV+CQok4F/T
-----END THIS IS NOT A REAL PRIVATE KEY-----


-----BEGIN CERTIFICATE-----
MIIC9TCCAl6gAwIBAgIJANL8E4epRNznMA0GCSqGSIb3DQEBBQUAMFsxGDAWBgNV
BAoTD1N1cGVyZmlzaCwgSW5jLjELMAkGA1UEBxMCU0YxCzAJBgNVBAgTAkNBMQsw
CQYDVQQGEwJVUzEYMBYGA1UEAxMPU3VwZXJmaXNoLCBJbmMuMB4XDTE0MDUxMjE2
MjUyNloXDTM0MDUwNzE2MjUyNlowWzEYMBYGA1UEChMPU3VwZXJmaXNoLCBJbmMu
MQswCQYDVQQHEwJTRjELMAkGA1UECBMCQ0ExCzAJBgNVBAYTAlVTMRgwFgYDVQQD
Ew9TdXBlcmZpc2gsIEluYy4wgZ8wDQYJKoZIhvcNAQEBBQADgY0AMIGJAoGBAOjz
Shh2Xxk/sc9Y6X9DBwmVgDXFD/5xMSeBmRImIKXfj2r8QlU57gk4idngNsSsAYJb
1Tnm+Y8HiN/+7vahFM6pdEXY/fAXVyqC4XouEpNarIrXFWPRt5tVgA9YvBxJ7SBi
3bZMpTrrHD2g/3pxptMQeDOuS8Ic/ZJKocPnQaQtAgMBAAGjgcAwgb0wDAYDVR0T
BAUwAwEB/zAdBgNVHQ4EFgQU+5izU38URC7o7tUJml4OVoaoNYgwgY0GA1UdIwSB
hTCBgoAU+5izU38URC7o7tUJml4OVoaoNYihX6RdMFsxGDAWBgNVBAoTD1N1cGVy
ZmlzaCwgSW5jLjELMAkGA1UEBxMCU0YxCzAJBgNVBAgTAkNBMQswCQYDVQQGEwJV
UzEYMBYGA1UEAxMPU3VwZXJmaXNoLCBJbmMuggkA0vwTh6lE3OcwDQYJKoZIhvcN
AQEFBQADgYEApHyg7ApKx3DEcWjzOyLi3JyN0JL+c35yK1VEmxu0Qusfr76645Oj
1IsYwpTws6a9ZTRMzST4GQvFFQra81eLqYbPbMPuhC+FCxkUF5i0DNSWi+kczJXJ
TtCqSwGl9t9JEoFqvtW+znZ9TqyLiOMw7TGEUI+88VAqW0qmXnwPcfo=
-----END CERTIFICATE-----

`;

const String SIMPLE_MULTI =
`-----BEGIN CERTIFICATE-----
MIIC9TCCAl6gAwIBAgIJANL8E4epRNznMA0GCSqGSIb3DQEBBQUAMFsxGDAWBgNV
BAoTD1N1cGVyZmlzaCwgSW5jLjELMAkGA1UEBxMCU0YxCzAJBgNVBAgTAkNBMQsw
CQYDVQQGEwJVUzEYMBYGA1UEAxMPU3VwZXJmaXNoLCBJbmMuMB4XDTE0MDUxMjE2
MjUyNloXDTM0MDUwNzE2MjUyNlowWzEYMBYGA1UEChMPU3VwZXJmaXNoLCBJbmMu
MQswCQYDVQQHEwJTRjELMAkGA1UECBMCQ0ExCzAJBgNVBAYTAlVTMRgwFgYDVQQD
Ew9TdXBlcmZpc2gsIEluYy4wgZ8wDQYJKoZIhvcNAQEBBQADgY0AMIGJAoGBAOjz
Shh2Xxk/sc9Y6X9DBwmVgDXFD/5xMSeBmRImIKXfj2r8QlU57gk4idngNsSsAYJb
1Tnm+Y8HiN/+7vahFM6pdEXY/fAXVyqC4XouEpNarIrXFWPRt5tVgA9YvBxJ7SBi
3bZMpTrrHD2g/3pxptMQeDOuS8Ic/ZJKocPnQaQtAgMBAAGjgcAwgb0wDAYDVR0T
BAUwAwEB/zAdBgNVHQ4EFgQU+5izU38URC7o7tUJml4OVoaoNYgwgY0GA1UdIwSB
hTCBgoAU+5izU38URC7o7tUJml4OVoaoNYihX6RdMFsxGDAWBgNVBAoTD1N1cGVy
ZmlzaCwgSW5jLjELMAkGA1UEBxMCU0YxCzAJBgNVBAgTAkNBMQswCQYDVQQGEwJV
UzEYMBYGA1UEAxMPU3VwZXJmaXNoLCBJbmMuggkA0vwTh6lE3OcwDQYJKoZIhvcN
AQEFBQADgYEApHyg7ApKx3DEcWjzOyLi3JyN0JL+c35yK1VEmxu0Qusfr76645Oj
1IsYwpTws6a9ZTRMzST4GQvFFQra81eLqYbPbMPuhC+FCxkUF5i0DNSWi+kczJXJ
TtCqSwGl9t9JEoFqvtW+znZ9TqyLiOMw7TGEUI+88VAqW0qmXnwPcfo=
-----END CERTIFICATE-----
-----BEGIN THING-----
MIICxjBABgkqhkiG9w0BBQ0wMzAbBgkqhkiG9w0BBQwwDgQIDHHhyAEZQoICAggA
MBQGCCqGSIb3DQMHBAiHEg+MCYQ30ASCAoDEvGvFRHvtWOb5Rc0f3lbVKqeUvWSz
xQn+rZELHnwb6baolmbFcsi6XkacVzL/EF7Ll4de/CSQ6pZZCCvfDzov0mPOuGve
SAe7hbAcol7+JWVfzbnVTblPf0i7mwSvK61cKq7YfcKJ2os/uJGpeX9zraywWyFx
f+EdTr348dOez8uHkURyY1cvSHsIdITALkChOonAYT68SVighTeB6xOCwfmsHx+X
3Qbhom2YCIxfJiaAoz2/LndCpDaEfOrVrxXFOKXrIbmeDEyjDQj16AVni9uuaj7l
NiO3zrrqxsfdVINPaAYRKQnS102jXqkH01z72c/MpMMC6dwZswF5V3R7RSXngyBn
1GLxVFHKR753Gt0IDag13Bd8Jt890/v0tE0Kx66jCkRGn+VCq6+bsnh7VpTH/cG5
dlFnv56lv2leknu5ghdJHX8YQ6HjnioaaheLA+ORAxqAlD8Itt1/pRBOOMSkutdz
d1px9dB2ZBpSoRAOcBwU5aFaw9uu+tXyzrPM3tZomu8ryQYMNlmVgPNDJOz6jPJi
jaZHWTS7U6j370oH/B0KTUG/ybrJGFnOmPP4h2u/ugG75EkfotURsvbrWuetQhOi
TCH+9nbIcT3pxnTXqI2IRHZXMturQ+6fqlJF3bb9bWarMBuC3KgprqyqXxeM0Sqg
VlyKLWwAuMf2Ec7t7ujqaNmVgv6bpwHEbR6njIi7lC7j4w6D2YQ8vacgvS3MB/K0
SX54HNVBVuXhAixPtYJ6tOBGm7QFAKaXju0PJ+AljnMEsHRekOs2u42OHBXEWDE8
VHw7/lTXWsJkBcQM+g/svyqV4xKHDAixPms2SUwJyKjvEgV+CQok4F/T
-----END THING-----
`;

const String SINGLE_WITH_HEADERS =
`-----BEGIN CERTIFICATE-----
My-Header: Example
This-is-a-longer-header: with some extra content that will bleed
 over to the next line.
Last: Header is single-line.

MIIC9TCCAl6gAwIBAgIJANL8E4epRNznMA0GCSqGSIb3DQEBBQUAMFsxGDAWBgNV
BAoTD1N1cGVyZmlzaCwgSW5jLjELMAkGA1UEBxMCU0YxCzAJBgNVBAgTAkNBMQsw
CQYDVQQGEwJVUzEYMBYGA1UEAxMPU3VwZXJmaXNoLCBJbmMuMB4XDTE0MDUxMjE2
MjUyNloXDTM0MDUwNzE2MjUyNlowWzEYMBYGA1UEChMPU3VwZXJmaXNoLCBJbmMu
MQswCQYDVQQHEwJTRjELMAkGA1UECBMCQ0ExCzAJBgNVBAYTAlVTMRgwFgYDVQQD
Ew9TdXBlcmZpc2gsIEluYy4wgZ8wDQYJKoZIhvcNAQEBBQADgY0AMIGJAoGBAOjz
Shh2Xxk/sc9Y6X9DBwmVgDXFD/5xMSeBmRImIKXfj2r8QlU57gk4idngNsSsAYJb
1Tnm+Y8HiN/+7vahFM6pdEXY/fAXVyqC4XouEpNarIrXFWPRt5tVgA9YvBxJ7SBi
3bZMpTrrHD2g/3pxptMQeDOuS8Ic/ZJKocPnQaQtAgMBAAGjgcAwgb0wDAYDVR0T
BAUwAwEB/zAdBgNVHQ4EFgQU+5izU38URC7o7tUJml4OVoaoNYgwgY0GA1UdIwSB
hTCBgoAU+5izU38URC7o7tUJml4OVoaoNYihX6RdMFsxGDAWBgNVBAoTD1N1cGVy
ZmlzaCwgSW5jLjELMAkGA1UEBxMCU0YxCzAJBgNVBAgTAkNBMQswCQYDVQQGEwJV
UzEYMBYGA1UEAxMPU3VwZXJmaXNoLCBJbmMuggkA0vwTh6lE3OcwDQYJKoZIhvcN
AQEFBQADgYEApHyg7ApKx3DEcWjzOyLi3JyN0JL+c35yK1VEmxu0Qusfr76645Oj
1IsYwpTws6a9ZTRMzST4GQvFFQra81eLqYbPbMPuhC+FCxkUF5i0DNSWi+kczJXJ
TtCqSwGl9t9JEoFqvtW+znZ9TqyLiOMw7TGEUI+88VAqW0qmXnwPcfo=
-----END CERTIFICATE-----
`;

const char[] SINGLE_DATA =
	x'308202f53082025ea003020102020900d2fc1387a944dce7300d06092a864886f70d0101050500305b31183016060355040a130f5375706572666973682c2049'
	x'6e632e310b3009060355040713025346310b3009060355040813024341310b3009060355040613025553311830160603550403130f5375706572666973682c20'
	x'496e632e301e170d3134303531323136323532365a170d3334303530373136323532365a305b31183016060355040a130f5375706572666973682c20496e632e'
	x'310b3009060355040713025346310b3009060355040813024341310b3009060355040613025553311830160603550403130f5375706572666973682c20496e63'
	x'2e30819f300d06092a864886f70d010101050003818d0030818902818100e8f34a18765f193fb1cf58e97f430709958035c50ffe7131278199122620a5df8f6a'
	x'fc425539ee093889d9e036c4ac01825bd539e6f98f0788dffeeef6a114cea97445d8fdf017572a82e17a2e12935aac8ad71563d1b79b55800f58bc1c49ed2062'
	x'ddb64ca53aeb1c3da0ff7a71a6d3107833ae4bc21cfd924aa1c3e741a42d0203010001a381c03081bd300c0603551d13040530030101ff301d0603551d0e0416'
	x'0414fb98b3537f14442ee8eed5099a5e0e5686a8358830818d0603551d230481853081828014fb98b3537f14442ee8eed5099a5e0e5686a83588a15fa45d305b'
	x'31183016060355040a130f5375706572666973682c20496e632e310b3009060355040713025346310b3009060355040813024341310b30090603550406130255'
	x'53311830160603550403130f5375706572666973682c20496e632e820900d2fc1387a944dce7300d06092a864886f70d010105050003818100a47ca0ec0a4ac7'
	x'70c47168f33b22e2dc9c8dd092fe737e722b55449b1bb442eb1fafbebae393a3d48b18c294f0b3a6bd65344ccd24f8190bc5150adaf3578ba986cf6cc3ee842f'
	x'850b19141798b40cd4968be91ccc95c94ed0aa4b01a5f6df4912816abed5bece767d4eac8b88e330ed3184508fbcf1502a5b4aa65e7c0f71fa';

const char[] MULTI_DATA =
	x'308202c6304006092a864886f70d01050d3033301b06092a864886f70d01050c300e04080c71e1c80119428202020800301406082a864886f70d030704088712'
	x'0f8c098437d004820280c4bc6bc5447bed58e6f945cd1fde56d52aa794bd64b3c509fead910b1e7c1be9b6a89666c572c8ba5e469c5732ff105ecb97875efc24'
	x'90ea9659082bdf0f3a2fd263ceb86bde4807bb85b01ca25efe25655fcdb9d54db94f7f48bb9b04af2bad5c2aaed87dc289da8b3fb891a9797f73adacb05b2171'
	x'7fe11d4ebdf8f1d39ecfcb8791447263572f487b087484c02e40a13a89c0613ebc4958a0853781eb1382c1f9ac1f1f97dd06e1a26d98088c5f262680a33dbf2e'
	x'7742a436847cead5af15c538a5eb21b99e0c4ca30d08f5e805678bdbae6a3ee53623b7cebaeac6c7dd54834f6806112909d2d74da35ea907d35cfbd9cfcca4c3'
	x'02e9dc19b3017957747b4525e7832067d462f15451ca47be771add080da835dc177c26df3dd3fbf4b44d0ac7aea30a44469fe542abaf9bb2787b5694c7fdc1b9'
	x'765167bf9ea5bf695e927bb98217491d7f1843a1e39e2a1a6a178b03e391031a80943f08b6dd7fa5104e38c4a4bad773775a71f5d076641a52a1100e701c14e5'
	x'a15ac3dbaefad5f2ceb3ccded6689aef2bc9060c36599580f34324ecfa8cf2628da6475934bb53a8f7ef4a07fc1d0a4d41bfc9bac91859ce98f3f8876bbfba01'
	x'bbe4491fa2d511b2f6eb5ae7ad4213a24c21fef676c8713de9c674d7a88d8844765732dbab43ee9faa5245ddb6fd6d66ab301b82dca829aeacaa5f178cd12aa0'
	x'565c8a2d6c00b8c7f611ceedeee8ea68d99582fe9ba701c46d1ea78c88bb942ee3e30e83d9843cbda720bd2dcc07f2b4497e781cd54156e5e1022c4fb5827ab4'
	x'e0469bb40500a6978eed0f27e0258e7304b0745e90eb36bb8d8e1c15c458313c547c3bfe54d75ac26405c40cfa0fecbf2a95e312870c08b13e6b36494c09c8a8'
	x'ef12057e090a24e05fd3';

const Pair{ String, fault }[] DECODE_ERRORS = {
	{ "heh", pem::INVALID_FORMAT },
	{ "---BEGIN CERT\nsomething\nsomething\nsomething", pem::INVALID_PRE_EB },
	{ "\n\n-----BEGIN SAMPLE-----\ntest: test\ntest1: test1\n\nQQ==\n", pem::MISSING_POST_EB },
	{ "\n\n-----BEGIN SAMPLE-----\ntest: test\ntest1: test1\n\nQQ==\n-----BEGIN TAG-----", pem::INVALID_POST_EB },
	{ "-----BEGIN SAMPLE-----\ntest: test\ntest1: test1\n\nQQ==\n-----END TAG-----", pem::MISMATCHED_TAG },
	{ "-----BEGIN -----\ntest: test\ntest1: test1\n\nQQ==\n-----END SAMPLE-----", pem::MISSING_TAG },
	{ "-----BEGIN SAMPLE-----\n   a key STARTING with spaces!: test\ntest1: test1\n\nQQ==\n-----END SAMPLE-----", pem::INVALID_HEADER },
	{ "-----BEGIN SAMPLE-----\nthis-line-has-no-colon-character\ntest1: test1\n\nQQ==\n-----END SAMPLE-----", pem::INVALID_BODY },
	{ "-----BEGIN SAMPLE-----\nthis-line-has-a-colon-character:butnospace\ntest1: test1\n\nQQ==\n-----END SAMPLE-----", pem::INVALID_BODY },
	{ "-----BEGIN SAMPLE-----\na key with spaces!: test\ntest1: test1\n\nQQ==\n-----END SAMPLE-----", pem::INVALID_HEADER_KEY },
	{ "-----BEGIN SAMPLE-----\n: nokey\ntest1: test1\n\nQQ==\n-----END SAMPLE-----", pem::INVALID_HEADER },   // because ':' isn't in the valid charset, which is checked first
	{ "-----BEGIN SAMPLE-----\na key with spaces!: \ntest1: test1\n\nQQ==\n-----END SAMPLE-----", pem::INVALID_BODY },   // because the whitespace is trimmed
	{ "-----BEGIN SAMPLE-----\ntest1: test1\nQQ==\n-----END SAMPLE-----", pem::INVALID_FORMAT },   // missing a blank line between headers/body
	{ "-----BEGIN SAMPLE-----\ntest1: test1\n\n-----END SAMPLE-----", pem::MISSING_BODY },
	{ "-----BEGIN SAMPLE-----\ntest1: test1\n\nthis is not base64 data\n-----END SAMPLE-----", pem::INVALID_BODY },
};

module std::core::runtime_test;
import std::sort;

fn void cmp_unit() @test
{
	TestUnit[] list = {
    	{ .name = "http::url_test::url_query" },
		{ .name = "http::url_test::url_init" },
    	{ .name = "http::url_test::url_decode" },
    	{ .name = "text_test::test_render_notag" },
    	{ .name = "text_test::test_render_tag1" },
    	{ .name = "text_test::test_render_template_iter" },
    	{ .name = "http::header_test::header_scan" },
    	{ .name = "http::header_test::header" },
    	{ .name = "stringmap_test::test_map" },
    	{ .name = "text_test::test_render_template" },
	};
	quicksort(list, &runtime::cmp_test_unit);

	String[] want = {
		"http::header_test::header",
        "http::header_test::header_scan",
        "http::url_test::url_decode",
        "http::url_test::url_init",
        "http::url_test::url_query",
        "stringmap_test::test_map",
        "text_test::test_render_notag",
        "text_test::test_render_tag1",
        "text_test::test_render_template",
        "text_test::test_render_template_iter",
	};
	assert(list.len == want.len);
	foreach (i, l : list)
	{
		assert(l.name == want[i], "got %s; want %s", l.name, want[i]);
	}
}
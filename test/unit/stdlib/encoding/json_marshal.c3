module json_marshal_test @test;
import std::encoding::json;
import std::collections::object;
import std::io;

enum Status
{
    ACTIVE,
    INACTIVE,
    PENDING
}

enum Priority
{
    LOW,
    MEDIUM,
    HIGH
}

enum State : int
{
    WAITING,
    RUNNING,
    TERMINATED
}

struct Address
{
    String street;
    String city;
    int zip_code;
}

struct Person
{
    String name;
    int age;
    bool is_active;
    double height;
    Status status;
}

struct ComplexData
{
    String text;
    int integer;
    float single_precision;
    double double_precision;
    bool flag;

    Status status;
    Priority priority;
    State current_state;

    Person owner;
    Address location;

    String[] tags;
    int[] numbers;
    float[] ratings;
    bool[] flags;

    Address[] offices;
    Person[] team_members;

    int[][] matrix;
    String[][] categories;
}

fn void test_comprehensive_marshaling() @test
{
    ComplexData data = {
        .text = "Hello \"World\"",
        .integer = 42,
        .single_precision = 3.14,
        .double_precision = 2.718281828,
        .flag = true,

        .status = Status.ACTIVE,
        .priority = Priority.HIGH,
        .current_state = State.RUNNING,

        .owner = {
            .name = "John Doe",
            .age = 30,
            .is_active = true,
            .height = 5.9,
            .status = Status.ACTIVE
        },
        .location = {
            .street = "123 Main St",
            .city = "New York",
            .zip_code = 10001
        },

        .tags = { "important", "urgent", "review" },
        .numbers = { 1, 2, 3, 4, 5 },
        .ratings = { 4.5, 3.8, 4.9 },
        .flags = { true, false, true },

        .offices = {
            {
                .street = "100 Tech Blvd",
                .city = "Austin",
                .zip_code = 78701
            },
            {
                .street = "200 Innovation Dr",
                .city = "Seattle",
                .zip_code = 98101
            }
        },
        .team_members = {
            {
                .name = "Alice Smith",
                .age = 28,
                .is_active = true,
                .height = 5.6,
                .status = Status.ACTIVE
            },
            {
                .name = "Bob Johnson",
                .age = 35,
                .is_active = false,
                .height = 6.1,
                .status = Status.INACTIVE
            }
        },

        .matrix = { { 1, 2, 3 }, { 4, 5, 6 }, { 7, 8, 9 } },
        .categories = { { "tech", "programming" }, { "business", "finance" } }
    };

    String json = json::marshal(mem, data)!!;
    defer free(json);

    Object* parsed = json::tparse(json)!!;

    test::eq(parsed.get_string("text")!!, "Hello \"World\"");
    test::eq(parsed.get_int("integer")!!, 42);
    test::eq((float)parsed.get_float("single_precision")!!, 3.14f);
    test::eq(parsed.get_bool("flag")!!, true);

    test::eq(parsed.get_string("status")!!, "ACTIVE");
    test::eq(parsed.get_string("priority")!!, "HIGH");
    test::eq(parsed.get_string("current_state")!!, "RUNNING");

    Object* owner = parsed.get("owner")!!;
    test::eq(owner.get_string("name")!!, "John Doe");
    test::eq(owner.get_int("age")!!, 30);
    test::eq(owner.get_bool("is_active")!!, true);
    test::eq((float)owner.get_float("height")!!, 5.9f);
    test::eq(owner.get_string("status")!!, "ACTIVE");

    Object* location = parsed.get("location")!!;
    test::eq(location.get_string("street")!!, "123 Main St");
    test::eq(location.get_string("city")!!, "New York");
    test::eq(location.get_int("zip_code")!!, 10001);

    Object* tags = parsed.get("tags")!!;
    test::eq(tags.get_len(), 3);
    test::eq(tags.get_string_at(0)!!, "important");
    test::eq(tags.get_string_at(1)!!, "urgent");
    test::eq(tags.get_string_at(2)!!, "review");

    Object* numbers = parsed.get("numbers")!!;
    test::eq(numbers.get_len(), 5);
    test::eq(numbers.get_int_at(0)!!, 1);
    test::eq(numbers.get_int_at(4)!!, 5);

    Object* ratings = parsed.get("ratings")!!;
    test::eq(ratings.get_len(), 3);
    test::eq((float)ratings.get_float_at(0)!!, 4.5f);
    test::eq((float)ratings.get_float_at(2)!!, 4.9f);

    Object* flags = parsed.get("flags")!!;
    test::eq(flags.get_len(), 3);
    test::eq(flags.get_bool_at(0)!!, true);
    test::eq(flags.get_bool_at(1)!!, false);
    test::eq(flags.get_bool_at(2)!!, true);

    Object* offices = parsed.get("offices")!!;
    test::eq(offices.get_len(), 2);

    Object* first_office = offices.get_at(0);
    test::eq(first_office.get_string("street")!!, "100 Tech Blvd");
    test::eq(first_office.get_string("city")!!, "Austin");
    test::eq(first_office.get_int("zip_code")!!, 78701);

    Object* second_office = offices.get_at(1);
    test::eq(second_office.get_string("street")!!, "200 Innovation Dr");
    test::eq(second_office.get_string("city")!!, "Seattle");
    test::eq(second_office.get_int("zip_code")!!, 98101);

    Object* team_members = parsed.get("team_members")!!;
    test::eq(team_members.get_len(), 2);

    Object* alice = team_members.get_at(0);
    test::eq(alice.get_string("name")!!, "Alice Smith");
    test::eq(alice.get_int("age")!!, 28);
    test::eq(alice.get_bool("is_active")!!, true);
    test::eq(alice.get_string("status")!!, "ACTIVE");

    Object* bob = team_members.get_at(1);
    test::eq(bob.get_string("name")!!, "Bob Johnson");
    test::eq(bob.get_int("age")!!, 35);
    test::eq(bob.get_bool("is_active")!!, false);
    test::eq(bob.get_string("status")!!, "INACTIVE");

    Object* matrix = parsed.get("matrix")!!;
    test::eq(matrix.get_len(), 3);

    Object* first_row = matrix.get_at(0);
    test::eq(first_row.get_len(), 3);
    test::eq(first_row.get_int_at(0)!!, 1);
    test::eq(first_row.get_int_at(1)!!, 2);
    test::eq(first_row.get_int_at(2)!!, 3);

    Object* categories = parsed.get("categories")!!;
    test::eq(categories.get_len(), 2);

    Object* first_category = categories.get_at(0);
    test::eq(first_category.get_len(), 2);
    test::eq(first_category.get_string_at(0)!!, "tech");
    test::eq(first_category.get_string_at(1)!!, "programming");

    test::@check(json.starts_with("{"));
    test::@check(json.ends_with("}"));
}

struct User
{
	String username;
	int created_at;
}

struct UserStats
{
	User user;
	int[] stars;
}

fn void compact_indent_output() @test
{
	User user = { .username = "admin", .created_at = 611667900 };
	test::eq(json::marshal(tmem, user)!!, `{"username":"admin","created_at":611667900}`);
	test::eq(json::marshal(tmem, user, NONE)!!, `{"username":"admin","created_at":611667900}`);
}

fn void space_1_indent_output() @test
{
	User user = { .username = "admin", .created_at = 611667900 };
	test::eq(json::marshal(tmem, user, SPACE_1)!!, "{\n \"username\": \"admin\",\n \"created_at\": 611667900\n}");
}

fn void space_2_indent_output() @test
{
	User user = { .username = "admin", .created_at = 611667900 };
	test::eq(json::marshal(tmem, user, SPACE_2)!!, "{\n  \"username\": \"admin\",\n  \"created_at\": 611667900\n}");
}

fn void space_3_indent_output() @test
{
	User user = { .username = "admin", .created_at = 611667900 };
	test::eq(json::marshal(tmem, user, SPACE_3)!!, "{\n   \"username\": \"admin\",\n   \"created_at\": 611667900\n}");
}

fn void space_4_indent_output() @test
{
	User user = { .username = "admin", .created_at = 611667900 };
	test::eq(json::marshal(tmem, user, SPACE_4)!!, "{\n    \"username\": \"admin\",\n    \"created_at\": 611667900\n}");
}

fn void space_5_indent_output() @test
{
	User user = { .username = "admin", .created_at = 611667900 };
	test::eq(json::marshal(tmem, user, SPACE_5)!!, "{\n     \"username\": \"admin\",\n     \"created_at\": 611667900\n}");
}

fn void tab_indent_output() @test
{
	User user = { .username = "admin", .created_at = 611667900 };
	test::eq(json::marshal(tmem, user, TAB)!!, "{\n\t\"username\": \"admin\",\n\t\"created_at\": 611667900\n}");
}

fn void compact_nested_indent_output() @test
{
	User user = { .username = "admin", .created_at = 611667900 };
	UserStats nested = { .user = user, .stars = { 1, 2 } };
	test::eq(json::marshal(tmem, nested, NONE)!!, `{"user":{"username":"admin","created_at":611667900},"stars":[1,2]}`);
}

fn void nested_space_indent_output() @test
{
	User user = { .username = "admin", .created_at = 611667900 };
	UserStats nested = { .user = user, .stars = { 1, 2 } };
	test::eq(json::marshal(tmem, nested, SPACE_2)!!, "{\n  \"user\": {\n    \"username\": \"admin\",\n    \"created_at\": 611667900\n  },\n  \"stars\": [\n    1,\n    2\n  ]\n}");
}

fn void nested_tab_indent_output() @test
{
	User user = { .username = "admin", .created_at = 611667900 };
	UserStats nested = { .user = user, .stars = { 1, 2 } };
	test::eq(json::marshal(tmem, nested, TAB)!!, "{\n\t\"user\": {\n\t\t\"username\": \"admin\",\n\t\t\"created_at\": 611667900\n\t},\n\t\"stars\": [\n\t\t1,\n\t\t2\n\t]\n}");
}

struct DoublePrecision
{
	double x;
}

struct FloatPrecision
{
	float x;
}

fn void double_roundtrip_pi() @test
{
	DoublePrecision v = { .x = 3.141592653589793 };
	String json = json::marshal(tmem, v)!!;
	test::eq(json::tparse(json)!!.get_float("x")!!, 3.141592653589793);
}

fn void double_roundtrip_max_significant() @test
{
	DoublePrecision v = { .x = 1.2345678901234567 };
	String json = json::marshal(tmem, v)!!;
	test::eq(json::tparse(json)!!.get_float("x")!!, 1.2345678901234567);
}

fn void double_roundtrip_small() @test
{
	DoublePrecision v = { .x = 1.7976931348623157e-300 };
	String json = json::marshal(tmem, v)!!;
	test::eq(json::tparse(json)!!.get_float("x")!!, 1.7976931348623157e-300);
}

fn void double_roundtrip_negative() @test
{
	DoublePrecision v = { .x = -2.2250738585072014e-308 };
	String json = json::marshal(tmem, v)!!;
	test::eq(json::tparse(json)!!.get_float("x")!!, -2.2250738585072014e-308);
}

fn void float_roundtrip_precise() @test
{
	FloatPrecision v = { .x = 1.23456789f };
	String json = json::marshal(tmem, v)!!;
	test::eq((float)json::tparse(json)!!.get_float("x")!!, 1.23456789f);
}

struct EscWrap
{
	String s;
}

macro String marshal_str(String s)
{
	EscWrap w = { .s = s };
	return json::marshal(tmem, w)!!;
}

fn void escape_short_sequences() @test
{
	test::eq(marshal_str("\"\\\b\f\n\r\t"), "{\"s\":\"\\\"\\\\\\b\\f\\n\\r\\t\"}");
}

fn void escape_low_control_as_u() @test
{
	// Control chars below 0x20 without a short escape become \u00xx.
	char[3] ctrl = { 0x00, 0x01, 0x1f };
	test::eq(marshal_str((String)ctrl[..]), `{"s":"\u0000\u0001\u001f"}`);
}

fn void escape_ascii_passthrough() @test
{
	test::eq(marshal_str("Hello, World! ~ / '"), `{"s":"Hello, World! ~ / '"}`);
}

fn void escape_del_passthrough() @test
{
	// DEL (0x7f) passes through unescaped.
	char[1] del = { 0x7f };
	char[9] expected = { '{', '"', 's', '"', ':', '"', 0x7f, '"', '}' };
	test::eq(marshal_str((String)del[..]), (String)expected[..]);
}

fn void escape_bmp_unicode_passthrough() @test
{
	test::eq(marshal_str("é€"), `{"s":"é€"}`);
}

fn void escape_astral_passthrough() @test
{
	test::eq(marshal_str("\U0001F600"), `{"s":"😀"}`);
}

fn void escape_astral_boundaries_passthrough() @test
{
	test::eq(marshal_str("\U00010000"), `{"s":"𐀀"}`);
	test::eq(marshal_str("\U0010FFFF"), `{"s":"􏿿"}`);
}

fn void escape_empty_string() @test
{
	test::eq(marshal_str(""), `{"s":""}`);
}

fn void escape_single_quote_not_escaped() @test
{
	// JSON has no \' escape; apostrophe must be emitted literally.
	test::eq(marshal_str("it's"), `{"s":"it's"}`);
}

fn void escape_solidus_not_escaped() @test
{
	test::eq(marshal_str("a/b"), `{"s":"a/b"}`);
}

fn void escape_roundtrip_mixed() @test
{
	String original = "Tab\tLF\nQuote\"Back\\Ctrl\x01Uni:é€\U0001F600End";
	String json = marshal_str(original);
	test::eq(json::tparse(json)!!.get_string("s")!!, original);
}

struct TestUser
{
	String first_name;
	String last_name;
}

struct TestScores
{
	int rank;
	int points;
}

fn void test_unmarshal_lifetime_bug()
{
	// Ensures each string field gets its own allocation, not a shared buffer.
	TestUser user = json::unmarshal(tmem, `{"first_name": "Alice", "last_name": "Bob"}`, TestUser)!!;
	test::eq(user.first_name, "Alice");
	test::eq(user.last_name, "Bob");
}

fn void test_unmarshal_number_desync()
{
	// Integers given as 1.0 or 1e2 in JSON must map cleanly to int fields.
	TestScores scores1 = json::unmarshal(tmem, `{"rank": 1.0, "points": 100}`, TestScores)!!;
	test::eq(scores1.rank, 1);
	test::eq(scores1.points, 100);

	TestScores scores2 = json::unmarshal(tmem, `{"rank": 1e2, "points": 100}`, TestScores)!!;
	test::eq(scores2.rank, 100);
	test::eq(scores2.points, 100);
}


struct LogEntry
{
	Status status;
	int code;
}

struct ServerConfig
{
	String host;
	int[] ports;
	LogEntry log;
}

fn void test_unmarshal_nested_and_slices()
{
	ServerConfig config = json::unmarshal(tmem, `
	{
		"host": "localhost",
		"ports": [80, 443, 8080],
		"log": {
			"status": "ACTIVE",
			"code": 200
		}
	}`, ServerConfig)!!;

	test::eq(config.host, "localhost");
	test::eq(config.ports.len, 3);
	test::eq(config.ports[0], 80);
	test::eq(config.ports[1], 443);
	test::eq(config.ports[2], 8080);
	test::eq(config.log.status, Status.ACTIVE);
	test::eq(config.log.code, 200);
}

fn void test_unmarshal_enum_representations()
{
	// Enums can be deserialized from their string name or their ordinal integer.
	LogEntry entry1 = json::unmarshal(tmem, `{"status": "PENDING", "code": 1}`, LogEntry)!!;
	test::eq(entry1.status, Status.PENDING);

	// ordinal 1 = INACTIVE in this module's Status enum
	LogEntry entry2 = json::unmarshal(tmem, `{"status": 1, "code": 2}`, LogEntry)!!;
	test::eq(entry2.status, Status.INACTIVE);
}

fn void test_unmarshal_null_and_missing_fields()
{
	ServerConfig config = json::unmarshal(tmem, `{"host": "new_host"}`, ServerConfig)!!;
	test::eq(config.host, "new_host");
	test::eq(config.ports.len, 0);
}

fn void test_marshal_unmarshal_roundtrip()
{
	ServerConfig original = {
		.host = "127.0.0.1",
		.ports = {8080, 9000},
		.log = { .status = Status.ACTIVE, .code = 201 }
	};

	String serialized = json::marshal(tmem, original)!!;
	ServerConfig deserialized = json::unmarshal(tmem, serialized, ServerConfig)!!;

	test::eq(deserialized.host, original.host);
	test::eq(deserialized.ports.len, original.ports.len);
	test::eq(deserialized.ports[0], original.ports[0]);
	test::eq(deserialized.ports[1], original.ports[1]);
	test::eq(deserialized.log.status, original.log.status);
	test::eq(deserialized.log.code, original.log.code);
}

struct EscapeData
{
	String text;
}

fn void test_unmarshal_from_stream()
{
	// Uses the InStream overload of unmarshal.
	ByteReader reader;
	reader.init(`{"host": "10.0.0.1", "ports": [22, 80], "log": {"status": "ACTIVE", "code": 200}}`);

	ServerConfig config = json::unmarshal(tmem, &reader, ServerConfig)!!;

	test::eq(config.host, "10.0.0.1");
	test::eq(config.ports.len, 2);
	test::eq(config.ports[0], 22);
	test::eq(config.ports[1], 80);
	test::eq(config.log.status, Status.ACTIVE);
	test::eq(config.log.code, 200);
}

fn void test_unmarshal_to_direct_dom()
{
	Object* dom = json::tparse(`{"host": "domain.com", "ports": [443], "log": {"status": "PENDING", "code": 500}}`)!!;

	ServerConfig config;
	json::@unmarshal_to(tmem, dom, config);

	test::eq(config.host, "domain.com");
	test::eq(config.ports.len, 1);
	test::eq(config.ports[0], 443);
	test::eq(config.log.status, Status.PENDING);
	test::eq(config.log.code, 500);
}

fn void test_unmarshal_empty_collections()
{
	ServerConfig config = json::unmarshal(tmem, `{"host": "", "ports": []}`, ServerConfig)!!;
	test::eq(config.host, "");
	test::eq(config.ports.len, 0);
}

fn void test_unmarshal_escapes()
{
	EscapeData data = json::unmarshal(tmem, `{"text": "Line1\nLine2\tTabbed\"Quoted\""}`, EscapeData)!!;
	test::eq(data.text, "Line1\nLine2\tTabbed\"Quoted\"");
}

struct SimpleKept
{
	int kept;
}

fn void test_skip_nested_curly_in_string()
{
	// A "}" inside an ignored string value must not terminate the object early.
	SimpleKept res = json::unmarshal(tmem, `{"ignored": {"key": "}"}, "kept": 42}`, SimpleKept)!!;
	test::eq(res.kept, 42);
}

fn void test_marshal_top_level_slice()
{
	int[] data = {1, 2, 3};
	String serialized = json::marshal(tmem, data)!!;
	test::eq(serialized, `[1,2,3]`);

	int[] deserialized = json::unmarshal(tmem, serialized, int[])!!;
	test::eq(deserialized.len, 3);
	test::eq(deserialized[0], 1);
	test::eq(deserialized[1], 2);
	test::eq(deserialized[2], 3);
}

struct FloatWrap
{
	float val;
}

fn void test_marshal_invalid_floats()
{
	FloatWrap nan_val = { .val = float::nan };
	FloatWrap inf_val = { .val = float::inf };

	@pool()
	{
		DString s;
		s.tinit();
		test::@error(json::marshal_to(&s, nan_val), json::INVALID_NUMBER);
		test::@error(json::marshal_to(&s, inf_val), json::INVALID_NUMBER);
	};
}

struct UnsignedWrap
{
	uint u32;
	ulong u64;
}

fn void test_unsigned_integers()
{
	UnsignedWrap wrap = { .u32 = 4294967295, .u64 = 9007199254740991 };
	String json = json::marshal(tmem, wrap)!!;
	test::eq(json, `{"u32":4294967295,"u64":9007199254740991}`);

	UnsignedWrap unwrapped = json::unmarshal(tmem, json, UnsignedWrap)!!;
	test::eq(unwrapped.u32, (uint)4294967295);
	test::eq(unwrapped.u64, (ulong)9007199254740991);
}

struct SimpleKeptWithComment
{
	int kept;
}

fn void test_skip_nested_comment_in_unmapped_field() @test
{
	SimpleKeptWithComment res = json::unmarshal(tmem, `
	{
		"ignored": {
			"key": 1
			// }
		},
		"kept": 42
	}`, SimpleKeptWithComment)!!;
	test::eq(res.kept, 42);
}

struct TagWrapper
{
	String[] tags;
}

fn void test_unmarshal_array_with_special_strings() @test
{
	// Commas, brackets, and braces inside string literals must be ignored
	TagWrapper res = json::unmarshal(tmem, `{"tags": ["foo,bar", "baz [ hello ]", "normal"]}`, TagWrapper)!!;
	test::eq(res.tags.len, 3);
	test::eq(res.tags[0], "foo,bar");
	test::eq(res.tags[1], "baz [ hello ]");
	test::eq(res.tags[2], "normal");
}

fn void test_unmarshal_array_with_comments_and_braces() @test
{
	// Comments containing braces/brackets/commas must be ignored
	TagWrapper res = json::unmarshal(tmem, `
	{
		"tags": [
			"foo",
			// comment with comma, brace { and bracket [
			"bar"
		]
	}`, TagWrapper)!!;
	test::eq(res.tags.len, 2);
	test::eq(res.tags[0], "foo");
	test::eq(res.tags[1], "bar");
}

fn void test_unmarshal_array_with_trailing_comma() @test
{
	// Trailing commas in arrays must not increase len or cause parsing errors
	TagWrapper res = json::unmarshal(tmem, `{"tags": ["one", "two",]}`, TagWrapper)!!;
	test::eq(res.tags.len, 2);
	test::eq(res.tags[0], "one");
	test::eq(res.tags[1], "two");
}

fn void test_unmarshal_trailing_garbage_rejected() @test
{
	test::@error(json::unmarshal(tmem, `{"username": "admin", "created_at": 123} trailing_garbage`, User), json::UNEXPECTED_CHARACTER);
}

struct SimpleRank
{
	int rank;
}

fn void test_unmarshal_leading_plus_rejected() @test
{
	test::@error(json::unmarshal(tmem, `{"rank": +123}`, SimpleRank), json::INVALID_NUMBER);
}

struct VectorWrapper
{
	int[<4>] vec;
}

fn void test_unmarshal_vector() @test
{
	VectorWrapper res = json::unmarshal(tmem, `{"vec": [10, 20, 30, 40]}`, VectorWrapper)!!;
	test::eq(res.vec[0], 10);
	test::eq(res.vec[1], 20);
	test::eq(res.vec[2], 30);
	test::eq(res.vec[3], 40);
}

fn void test_unmarshal_vector_with_oversized_json() @test
{
	// Ensures that more elements in the JSON array are safely ignored and do not trigger out-of-bounds panic
	VectorWrapper res = json::unmarshal(tmem, `{"vec": [10, 20, 30, 40, 50, 60]}`, VectorWrapper)!!;
	test::eq(res.vec[0], 10);
	test::eq(res.vec[3], 40);
}

struct BigIntRec
{
	long id;
	long ts;
}

fn void test_unmarshal_large_int_precision() @test
{
	BigIntRec r = json::unmarshal(tmem, `{"id": 9007199254740993, "ts": 1600000000123456789}`, BigIntRec)!!;
	test::eq(r.id, 9007199254740993);
	test::eq(r.ts, 1600000000123456789);
}

enum RegColor
{
	RED,
	GREEN,
	BLUE
}

struct ColorRec
{
	RegColor c;
}

fn void test_unmarshal_enum_out_of_range_faults() @test
{
	if (catch json::unmarshal(tmem, `{"c": 99}`, ColorRec)) return;
	assert(false, "out-of-range enum ordinal should return a fault, not succeed or panic");
}

fn void test_unmarshal_unknown_enum_string_faults() @test
{
	if (catch json::unmarshal(tmem, `{"c": "PURPLE"}`, ColorRec)) return;
	assert(false, "unknown enum string should fault, not silently map to the first member");
}
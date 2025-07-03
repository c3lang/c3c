module json_marshal_test @test;
import std::encoding::json;
import std::collections::object;
import std::io;

// Test enums
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

// Enum with associated value
enum State : int (String description)
{
    WAITING = "waiting",
    RUNNING = "running",
    TERMINATED = "ended"
}

// Basic structures
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

// Complex structure with all features
struct ComplexData
{
    // Primitive types
    String text;
    int integer;
    float single_precision;
    double double_precision;
    bool flag;

    // Enums
    Status status;
    Priority priority;
    State current_state;

    // Nested struct
    Person owner;
    Address location;

    // Arrays of primitives
    String[] tags;
    int[] numbers;
    float[] ratings;
    bool[] flags;

    // Array of structs
    Address[] offices;
    Person[] team_members;

    // Array of arrays
    int[][] matrix;
    String[][] categories;
}

fn void test_comprehensive_marshaling() @test
{
    // Create a complex data structure with all supported features
    ComplexData data = {
        // Primitive types
        .text = "Hello \"World\"",
        .integer = 42,
        .single_precision = 3.14,
        .double_precision = 2.718281828,
        .flag = true,

        // Enums
        .status = Status.ACTIVE,
        .priority = Priority.HIGH,
        .current_state = State.RUNNING,

        // Nested structs
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

        // Arrays of primitives
        .tags = { "important", "urgent", "review" },
        .numbers = { 1, 2, 3, 4, 5 },
        .ratings = { 4.5, 3.8, 4.9 },
        .flags = { true, false, true },

        // Array of structs
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

        // Array of arrays
        .matrix = { { 1, 2, 3 }, { 4, 5, 6 }, { 7, 8, 9 } },
        .categories = { { "tech", "programming" }, { "business", "finance" } }
    };

    // Marshal the complex data structure
    String json = json::marshal(mem, data)!!;
    defer free(json);

    // Parse the marshaled JSON back to verify it's valid and correct
    Object* parsed = json::tparse_string(json)!!;

    // Verify primitive types
    assert(parsed.get_string("text")!! == "Hello \"World\"");
    assert(parsed.get_int("integer")!! == 42);
    assert(parsed.get_float("single_precision")!! == 3.14f);
    assert(parsed.get_bool("flag")!! == true);

    // Verify enums (marshaled as strings)
    assert(parsed.get_string("status")!! == "ACTIVE");
    assert(parsed.get_string("priority")!! == "HIGH");
    assert(parsed.get_string("current_state")!! == "RUNNING");

    // Verify nested structs
    Object* owner = parsed.get("owner")!!;
    assert(owner.get_string("name")!! == "John Doe");
    assert(owner.get_int("age")!! == 30);
    assert(owner.get_bool("is_active")!! == true);
    assert(owner.get_float("height")!! == 5.9f);
    assert(owner.get_string("status")!! == "ACTIVE");

    Object* location = parsed.get("location")!!;
    assert(location.get_string("street")!! == "123 Main St");
    assert(location.get_string("city")!! == "New York");
    assert(location.get_int("zip_code")!! == 10001);

    // Verify arrays of primitives
    Object* tags = parsed.get("tags")!!;
    assert(tags.get_len() == 3);
    assert(tags.get_string_at(0)!! == "important");
    assert(tags.get_string_at(1)!! == "urgent");
    assert(tags.get_string_at(2)!! == "review");

    Object* numbers = parsed.get("numbers")!!;
    assert(numbers.get_len() == 5);
    assert(numbers.get_int_at(0)!! == 1);
    assert(numbers.get_int_at(4)!! == 5);

    Object* ratings = parsed.get("ratings")!!;
    assert(ratings.get_len() == 3);
    assert(ratings.get_float_at(0)!! == 4.5f);
    assert(ratings.get_float_at(2)!! == 4.9f);

    Object* flags = parsed.get("flags")!!;
    assert(flags.get_len() == 3);
    assert(flags.get_bool_at(0)!! == true);
    assert(flags.get_bool_at(1)!! == false);
    assert(flags.get_bool_at(2)!! == true);

    // Verify array of structs
    Object* offices = parsed.get("offices")!!;
    assert(offices.get_len() == 2);

    Object* first_office = offices.get_at(0);
    assert(first_office.get_string("street")!! == "100 Tech Blvd");
    assert(first_office.get_string("city")!! == "Austin");
    assert(first_office.get_int("zip_code")!! == 78701);

    Object* second_office = offices.get_at(1);
    assert(second_office.get_string("street")!! == "200 Innovation Dr");
    assert(second_office.get_string("city")!! == "Seattle");
    assert(second_office.get_int("zip_code")!! == 98101);

    Object* team_members = parsed.get("team_members")!!;
    assert(team_members.get_len() == 2);

    Object* alice = team_members.get_at(0);
    assert(alice.get_string("name")!! == "Alice Smith");
    assert(alice.get_int("age")!! == 28);
    assert(alice.get_bool("is_active")!! == true);
    assert(alice.get_string("status")!! == "ACTIVE");

    Object* bob = team_members.get_at(1);
    assert(bob.get_string("name")!! == "Bob Johnson");
    assert(bob.get_int("age")!! == 35);
    assert(bob.get_bool("is_active")!! == false);
    assert(bob.get_string("status")!! == "INACTIVE");

    // Verify array of arrays
    Object* matrix = parsed.get("matrix")!!;
    assert(matrix.get_len() == 3);

    Object* first_row = matrix.get_at(0);
    assert(first_row.get_len() == 3);
    assert(first_row.get_int_at(0)!! == 1);
    assert(first_row.get_int_at(1)!! == 2);
    assert(first_row.get_int_at(2)!! == 3);

    Object* categories = parsed.get("categories")!!;
    assert(categories.get_len() == 2);

    Object* first_category = categories.get_at(0);
    assert(first_category.get_len() == 2);
    assert(first_category.get_string_at(0)!! == "tech");
    assert(first_category.get_string_at(1)!! == "programming");

    // Verify JSON format
    assert(json.starts_with("{"));
    assert(json.ends_with("}"));
}
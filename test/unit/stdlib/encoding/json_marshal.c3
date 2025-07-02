module json_marshal_test @test;
import std::encoding::json;
import std::collections::object;
import std::io;

// Test enums
enum Status
{
    ACTIVE,
    INACTIVE,
    PENDING,
    SUSPENDED
}

enum Priority
{
    LOW,
    MEDIUM,
    HIGH,
    CRITICAL
}

// Enum with single String associated value
enum State : int (String description)
{
    WAITING = "waiting",
    RUNNING = "running",
    TERMINATED = "ended"
}

// Enum with multiple associated values (marshaled using enum name)
enum ComplexState : int (String desc, bool active)
{
    IDLE = { "idle", false },
    BUSY = { "busy", true }
}

// Test structures with various primitive types
struct Person
{
    String name;
    int age;
    bool is_active;
    double height;
    Status status;
}

struct Product
{
    String title;
    int price;
    String category;
    float rating;
    bool in_stock;
}

// Test struct with all supported primitive types
struct AllTypes
{
    String text;
    int integer;
    float single_precision;
    double double_precision;
    bool flag;
}

// Nested struct test structures
struct Address
{
    String street;
    String city;
    int zip_code;
}

struct Company
{
    String name;
    Address headquarters;
    int employee_count;
}

struct Employee
{
    Person personal_info;
    Company employer;
    float salary;
    bool is_remote;
}

struct Department
{
    String name;
    String[] skills_required;
    Address[] office_locations;
    int budget;
    Priority priority;
}

struct Task
{
    String title;
    Status status;
    Priority priority;
    int estimated_hours;
}

struct Process
{
    String name;
    State current_state;
    ComplexState complex_state;
    int pid;
}

fn void test_primitive_marshaling() @test
{
    // Test integers
    String result = json::tmarshal_value(42)!!;
    assert(result == "42");
    
    // Test floats
    result = json::tmarshal_value(3.14)!!;
    assert(result.starts_with("3.14"));
    
    // Test doubles
    result = json::tmarshal_value(2.718281828)!!;
    assert(result.starts_with("2.718"));
    
    // Test booleans
    result = json::tmarshal_value(true)!!;
    assert(result == "true");
    
    result = json::tmarshal_value(false)!!;
    assert(result == "false");
    
    // Test strings
    result = json::tmarshal_value("hello world")!!;
    assert(result == `"hello world"`);
    
    // Test string with special characters
    result = json::tmarshal_value("Hello \"world\"")!!;
    assert(result == `"Hello \"world\""`);
}

fn void test_enum_marshaling() @test
{
    // Test individual enum values
    String result = json::tmarshal_value(Status.ACTIVE)!!;
    assert(result == `"ACTIVE"`);

    result = json::tmarshal_value(Status.INACTIVE)!!;
    assert(result == `"INACTIVE"`);

    result = json::tmarshal_value(Status.PENDING)!!;
    assert(result == `"PENDING"`);

    result = json::tmarshal_value(Status.SUSPENDED)!!;
    assert(result == `"SUSPENDED"`);

    // Test Priority enum
    result = json::tmarshal_value(Priority.LOW)!!;
    assert(result == `"LOW"`);

    result = json::tmarshal_value(Priority.MEDIUM)!!;
    assert(result == `"MEDIUM"`);

    result = json::tmarshal_value(Priority.HIGH)!!;
    assert(result == `"HIGH"`);

    result = json::tmarshal_value(Priority.CRITICAL)!!;
    assert(result == `"CRITICAL"`);
}

fn void test_enum_with_associated_values() @test
{
    // Test enum with single String associated value (always uses enum names)
    String result = json::tmarshal_value(State.WAITING)!!;
    assert(result == `"WAITING"`);

    result = json::tmarshal_value(State.RUNNING)!!;
    assert(result == `"RUNNING"`);

    result = json::tmarshal_value(State.TERMINATED)!!;
    assert(result == `"TERMINATED"`);

    // Test enum with multiple associated values (always uses enum names)
    result = json::tmarshal_value(ComplexState.IDLE)!!;
    assert(result == `"IDLE"`);

    result = json::tmarshal_value(ComplexState.BUSY)!!;
    assert(result == `"BUSY"`);
}

fn void test_struct_with_associated_value_enums() @test
{
    Process process = {
        .name = "web_server",
        .current_state = State.RUNNING,
        .complex_state = ComplexState.BUSY,
        .pid = 1234
    };

    String json = json::tmarshal(process)!!;

    // Check that enums always use the enum name
    assert(json.contains(`"name":"web_server"`));
    assert(json.contains(`"current_state":"RUNNING"`));  // Always uses enum name
    assert(json.contains(`"complex_state":"BUSY"`));     // Always uses enum name
    assert(json.contains(`"pid":1234`));

    // Should be valid JSON format
    assert(json.starts_with("{"));
    assert(json.ends_with("}"));
}

fn void test_struct_with_enums() @test
{
    Task task = {
        .title = "Implement JSON marshaling",
        .status = Status.ACTIVE,
        .priority = Priority.HIGH,
        .estimated_hours = 8
    };

    String json = json::tmarshal(task)!!;

    // Parse the marshaled JSON back to verify it's valid and correct
    Object* parsed = json::tparse_string(json)!!;

    // Verify all fields through the JSON Object API
    assert(parsed.get_string("title")!! == "Implement JSON marshaling");
    assert(parsed.get_string("status")!! == "ACTIVE");  // Enum marshaled as string
    assert(parsed.get_string("priority")!! == "HIGH");  // Enum marshaled as string
    assert(parsed.get_int("estimated_hours")!! == 8);

    // Should be valid JSON format
    assert(json.starts_with("{"));
    assert(json.ends_with("}"));
}

fn void test_simple_struct_marshaling() @test
{
    Person person = {
        .name = "John Doe",
        .age = 30,
        .is_active = true,
        .height = 5.9,
        .status = Status.ACTIVE
    };

    String json = json::tmarshal(person)!!;

    // Parse the marshaled JSON back to verify it's valid and correct
    Object* parsed = json::tparse_string(json)!!;

    // Verify all fields through the JSON Object API
    assert(parsed.get_string("name")!! == "John Doe");
    assert(parsed.get_int("age")!! == 30);
    assert(parsed.get_bool("is_active")!! == true);
    assert(parsed.get_float("height")!! == 5.9f);
    assert(parsed.get_string("status")!! == "ACTIVE");

    // Also keep the original string checks for format verification
    assert(json.starts_with("{"));
    assert(json.ends_with("}"));
}

fn void test_struct_with_multiple_fields() @test
{
    Product product = {
        .title = "C3 Programming Book",
        .price = 2999,
        .category = "Programming",
        .rating = 4.8,
        .in_stock = true
    };

    String json = json::tmarshal(product)!!;

    // Parse the marshaled JSON back to verify it's valid and correct
    Object* parsed = json::tparse_string(json)!!;

    // Verify all fields through the JSON Object API
    assert(parsed.get_string("title")!! == "C3 Programming Book");
    assert(parsed.get_int("price")!! == 2999);
    assert(parsed.get_string("category")!! == "Programming");
    assert(parsed.get_float("rating")!! == 4.8f);
    assert(parsed.get_bool("in_stock")!! == true);

    // Also keep the original string checks for format verification
    assert(json.starts_with("{"));
    assert(json.ends_with("}"));
}

fn void test_array_marshaling() @test
{
    // Test integer arrays
    int[] numbers = { 1, 2, 3, 4, 5 };
    String result = json::tmarshal_array(numbers)!!;

    // Parse and verify integer array
    Object* parsed_numbers = json::tparse_string(result)!!;
    assert(parsed_numbers.get_len() == 5);
    assert(parsed_numbers.get_int_at(0)!! == 1);
    assert(parsed_numbers.get_int_at(1)!! == 2);
    assert(parsed_numbers.get_int_at(2)!! == 3);
    assert(parsed_numbers.get_int_at(3)!! == 4);
    assert(parsed_numbers.get_int_at(4)!! == 5);

    // Test string arrays
    String[] words = { "hello", "world", "test" };
    result = json::tmarshal_array(words)!!;

    // Parse and verify string array
    Object* parsed_words = json::tparse_string(result)!!;
    assert(parsed_words.get_len() == 3);
    assert(parsed_words.get_string_at(0)!! == "hello");
    assert(parsed_words.get_string_at(1)!! == "world");
    assert(parsed_words.get_string_at(2)!! == "test");

    // Test float arrays
    float[] prices = { 1.99, 2.50, 3.14 };
    result = json::tmarshal_array(prices)!!;

    // Parse and verify float array
    Object* parsed_prices = json::tparse_string(result)!!;
    assert(parsed_prices.get_len() == 3);
    assert(parsed_prices.get_float_at(0)!! == 1.99f);
    assert(parsed_prices.get_float_at(1)!! == 2.50f);
    assert(parsed_prices.get_float_at(2)!! == 3.14f);

    // Test boolean arrays
    bool[] flags = { true, false, true };
    result = json::tmarshal_array(flags)!!;

    // Parse and verify boolean array
    Object* parsed_flags = json::tparse_string(result)!!;
    assert(parsed_flags.get_len() == 3);
    assert(parsed_flags.get_bool_at(0)!! == true);
    assert(parsed_flags.get_bool_at(1)!! == false);
    assert(parsed_flags.get_bool_at(2)!! == true);

    // Test enum arrays
    Status[] statuses = { Status.ACTIVE, Status.PENDING, Status.INACTIVE };
    result = json::tmarshal_array(statuses)!!;
    assert(result == `["ACTIVE","PENDING","INACTIVE"]`);

    Priority[] priorities = { Priority.LOW, Priority.HIGH };
    result = json::tmarshal_array(priorities)!!;
    assert(result == `["LOW","HIGH"]`);

    // Test enum arrays with associated values (always uses enum names)
    State[] states = { State.WAITING, State.RUNNING, State.TERMINATED };
    result = json::tmarshal_array(states)!!;
    assert(result == `["WAITING","RUNNING","TERMINATED"]`);  // Always uses enum names

    ComplexState[] complex_states = { ComplexState.IDLE, ComplexState.BUSY };
    result = json::tmarshal_array(complex_states)!!;
    assert(result == `["IDLE","BUSY"]`);  // Always uses enum names
}

fn void test_string_escaping() @test
{
    Person person = {
        .name = "John \"The Coder\" Doe",
        .age = 25,
        .is_active = true,
        .height = 5.8,
        .status = Status.ACTIVE
    };

    String json = json::tmarshal(person)!!;

    // Parse the marshaled JSON back to verify escaping works correctly
    Object* parsed = json::tparse_string(json)!!;

    // Verify that the original string with quotes is preserved correctly
    assert(parsed.get_string("name")!! == "John \"The Coder\" Doe");
    assert(parsed.get_int("age")!! == 25);
    assert(parsed.get_bool("is_active")!! == true);
    assert(parsed.get_float("height")!! == 5.8f);
    assert(parsed.get_string("status")!! == "ACTIVE");
}

fn void test_empty_strings() @test
{
    Person person = {
        .name = "",
        .age = 0,
        .is_active = false,
        .height = 0.0,
        .status = Status.INACTIVE
    };

    String json = json::tmarshal(person)!!;

    // Should handle empty strings and zero values properly
    assert(json.contains(`"name":""`));
    assert(json.contains(`"age":0`));
    assert(json.contains(`"is_active":false`));
    assert(json.contains(`"height":0`));
    assert(json.contains(`"status":"INACTIVE"`));
}

fn void test_all_primitive_types() @test
{
    AllTypes data = {
        .text = "test string",
        .integer = 42,
        .single_precision = 3.14,
        .double_precision = 2.718281828,
        .flag = true
    };
    
    String json = json::tmarshal(data)!!;

    // Verify all types are marshaled correctly
    assert(json.contains(`"text":"test string"`));
    assert(json.contains(`"integer":42`));
    assert(json.contains(`"single_precision":3.14`));
    assert(json.contains(`"double_precision":2.718`));
    assert(json.contains(`"flag":true`));
    
    // Should be valid JSON format
    assert(json.starts_with("{"));
    assert(json.ends_with("}"));
}

fn void test_nested_struct_marshaling() @test
{
    Address address = {
        .street = "123 Main St",
        .city = "New York",
        .zip_code = 10001
    };

    Company company = {
        .name = "Tech Corp",
        .headquarters = address,
        .employee_count = 500
    };

    String json = json::tmarshal(company)!!;

    // Parse the marshaled JSON back to verify it's valid and correct
    Object* parsed = json::tparse_string(json)!!;

    // Verify top-level fields
    assert(parsed.get_string("name")!! == "Tech Corp");
    assert(parsed.get_int("employee_count")!! == 500);

    // Verify nested struct fields
    Object* headquarters = parsed.get("headquarters")!!;
    assert(headquarters.get_string("street")!! == "123 Main St");
    assert(headquarters.get_string("city")!! == "New York");
    assert(headquarters.get_int("zip_code")!! == 10001);

    // Should be valid JSON format
    assert(json.starts_with("{"));
    assert(json.ends_with("}"));
}

fn void test_deeply_nested_struct_marshaling() @test
{
    Employee employee = {
        .personal_info = {
            .name = "Alice Johnson",
            .age = 28,
            .is_active = true,
            .height = 5.6,
            .status = Status.ACTIVE
        },
        .employer = {
            .name = "Innovation Labs",
            .headquarters = {
                .street = "456 Tech Ave",
                .city = "San Francisco",
                .zip_code = 94105
            },
            .employee_count = 250
        },
        .salary = 85000.0,
        .is_remote = true
    };

    String json = json::tmarshal(employee)!!;

    // Parse the marshaled JSON back to verify it's valid and correct
    Object* parsed = json::tparse_string(json)!!;

    // Verify top-level fields
    assert(parsed.get_float("salary")!! == 85000.0f);
    assert(parsed.get_bool("is_remote")!! == true);

    // Verify personal_info nested struct
    Object* personal_info = parsed.get("personal_info")!!;
    assert(personal_info.get_string("name")!! == "Alice Johnson");
    assert(personal_info.get_int("age")!! == 28);
    assert(personal_info.get_bool("is_active")!! == true);
    assert(personal_info.get_float("height")!! == 5.6f);
    assert(personal_info.get_string("status")!! == "ACTIVE");

    // Verify employer nested struct
    Object* employer = parsed.get("employer")!!;
    assert(employer.get_string("name")!! == "Innovation Labs");
    assert(employer.get_int("employee_count")!! == 250);

    // Verify deeply nested headquarters struct
    Object* headquarters = employer.get("headquarters")!!;
    assert(headquarters.get_string("street")!! == "456 Tech Ave");
    assert(headquarters.get_string("city")!! == "San Francisco");
    assert(headquarters.get_int("zip_code")!! == 94105);

    // Should be valid JSON format
    assert(json.starts_with("{"));
    assert(json.ends_with("}"));
}

fn void test_array_of_structs() @test
{
    Address[] addresses = {
        {
            .street = "100 First St",
            .city = "Boston",
            .zip_code = 2101
        },
        {
            .street = "200 Second Ave",
            .city = "Chicago",
            .zip_code = 60601
        }
    };

    String json = json::tmarshal_array(addresses)!!;

    // Parse the marshaled JSON back to verify it's valid and correct
    Object* parsed = json::tparse_string(json)!!;

    // Verify array structure and length
    assert(parsed.get_len() == 2);

    // Verify first address struct
    Object* first_address = parsed.get_at(0);
    assert(first_address.get_string("street")!! == "100 First St");
    assert(first_address.get_string("city")!! == "Boston");
    assert(first_address.get_int("zip_code")!! == 2101);

    // Verify second address struct
    Object* second_address = parsed.get_at(1);
    assert(second_address.get_string("street")!! == "200 Second Ave");
    assert(second_address.get_string("city")!! == "Chicago");
    assert(second_address.get_int("zip_code")!! == 60601);

    // Check array format
    assert(json.starts_with("["));
    assert(json.ends_with("]"));
}

fn void test_struct_with_nested_arrays() @test
{
    Department dept = {
        .name = "Engineering",
        .skills_required = { "C3", "Rust", "Go" },
        .office_locations = {
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
        .budget = 1000000,
        .priority = Priority.HIGH
    };

    String json = json::tmarshal(dept)!!;

    // Parse the marshaled JSON back to verify it's valid and correct
    Object* parsed = json::tparse_string(json)!!;

    // Verify top-level fields
    assert(parsed.get_string("name")!! == "Engineering");
    assert(parsed.get_int("budget")!! == 1000000);
    assert(parsed.get_string("priority")!! == "HIGH");

    // Verify skills_required string array
    Object* skills = parsed.get("skills_required")!!;
    assert(skills.get_len() == 3);
    assert(skills.get_string_at(0)!! == "C3");
    assert(skills.get_string_at(1)!! == "Rust");
    assert(skills.get_string_at(2)!! == "Go");

    // Verify office_locations struct array
    Object* locations = parsed.get("office_locations")!!;
    assert(locations.get_len() == 2);

    // Verify first office location
    Object* first_office = locations.get_at(0);
    assert(first_office.get_string("street")!! == "100 Tech Blvd");
    assert(first_office.get_string("city")!! == "Austin");
    assert(first_office.get_int("zip_code")!! == 78701);

    // Verify second office location
    Object* second_office = locations.get_at(1);
    assert(second_office.get_string("street")!! == "200 Innovation Dr");
    assert(second_office.get_string("city")!! == "Seattle");
    assert(second_office.get_int("zip_code")!! == 98101);

    // Should be valid JSON format
    assert(json.starts_with("{"));
    assert(json.ends_with("}"));
}

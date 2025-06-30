module json_marshal_test @test;
import std::encoding::json;
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
    String? result = json::marshal_value(42);
    assert(result!! == "42");
    
    // Test floats
    result = json::marshal_value(3.14);
    assert(result!!.starts_with("3.14"));
    
    // Test doubles
    result = json::marshal_value(2.718281828);
    assert(result!!.starts_with("2.718"));
    
    // Test booleans
    result = json::marshal_value(true);
    assert(result!! == "true");
    
    result = json::marshal_value(false);
    assert(result!! == "false");
    
    // Test strings
    result = json::marshal_value("hello world");
    assert(result!! == `"hello world"`);
    
    // Test string with special characters
    result = json::marshal_value("Hello \"world\"");
    assert(result!! == `"Hello \"world\""`);
}

fn void test_enum_marshaling() @test
{
    // Test individual enum values
    String? result = json::marshal_value(Status.ACTIVE);
    assert(result!! == `"ACTIVE"`);

    result = json::marshal_value(Status.INACTIVE);
    assert(result!! == `"INACTIVE"`);

    result = json::marshal_value(Status.PENDING);
    assert(result!! == `"PENDING"`);

    result = json::marshal_value(Status.SUSPENDED);
    assert(result!! == `"SUSPENDED"`);

    // Test Priority enum
    result = json::marshal_value(Priority.LOW);
    assert(result!! == `"LOW"`);

    result = json::marshal_value(Priority.MEDIUM);
    assert(result!! == `"MEDIUM"`);

    result = json::marshal_value(Priority.HIGH);
    assert(result!! == `"HIGH"`);

    result = json::marshal_value(Priority.CRITICAL);
    assert(result!! == `"CRITICAL"`);
}

fn void test_enum_with_associated_values() @test
{
    // Test enum with single String associated value (always uses enum names)
    String? result = json::marshal_value(State.WAITING);
    assert(result!! == `"WAITING"`);

    result = json::marshal_value(State.RUNNING);
    assert(result!! == `"RUNNING"`);

    result = json::marshal_value(State.TERMINATED);
    assert(result!! == `"TERMINATED"`);

    // Test enum with multiple associated values (always uses enum names)
    result = json::marshal_value(ComplexState.IDLE);
    assert(result!! == `"IDLE"`);

    result = json::marshal_value(ComplexState.BUSY);
    assert(result!! == `"BUSY"`);
}

fn void test_struct_with_associated_value_enums() @test
{
    Process process = {
        .name = "web_server",
        .current_state = State.RUNNING,
        .complex_state = ComplexState.BUSY,
        .pid = 1234
    };

    String? result = json::marshal(process);
    String json = result!!;

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

    String? result = json::marshal(task);
    String json = result!!;

    // Check that enum fields are marshaled as strings
    assert(json.contains(`"title":"Implement JSON marshaling"`));
    assert(json.contains(`"status":"ACTIVE"`));
    assert(json.contains(`"priority":"HIGH"`));
    assert(json.contains(`"estimated_hours":8`));

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

    String? result = json::marshal(person);
    String json = result!!;

    // Check that all fields are present
    assert(json.contains(`"name":"John Doe"`));
    assert(json.contains(`"age":30`));
    assert(json.contains(`"is_active":true`));
    assert(json.contains(`"height":5.9`));
    assert(json.contains(`"status":"ACTIVE"`));

    // Should start and end with braces
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
    
    String? result = json::marshal(product);
    String json = result!!;
    
    // Check that all fields are present
    assert(json.contains(`"title":"C3 Programming Book"`));
    assert(json.contains(`"price":2999`));
    assert(json.contains(`"category":"Programming"`));
    assert(json.contains(`"rating":4.8`));
    assert(json.contains(`"in_stock":true`));
    
    // Should start and end with braces
    assert(json.starts_with("{"));
    assert(json.ends_with("}"));
}

fn void test_array_marshaling() @test
{
    // Test integer arrays
    int[] numbers = { 1, 2, 3, 4, 5 };
    String? result = json::marshal_array(numbers);
    assert(result!! == "[1,2,3,4,5]");
    
    // Test string arrays
    String[] words = { "hello", "world", "test" };
    result = json::marshal_array(words);
    assert(result!! == `["hello","world","test"]`);
    
    // Test float arrays
    float[] prices = { 1.99, 2.50, 3.14 };
    result = json::marshal_array(prices);
    String json = result!!;
    assert(json.starts_with("["));
    assert(json.ends_with("]"));
    assert(json.contains("1.99"));
    assert(json.contains("2.5"));
    assert(json.contains("3.14"));
    
    // Test boolean arrays
    bool[] flags = { true, false, true };
    result = json::marshal_array(flags);
    assert(result!! == "[true,false,true]");

    // Test enum arrays
    Status[] statuses = { Status.ACTIVE, Status.PENDING, Status.INACTIVE };
    result = json::marshal_array(statuses);
    assert(result!! == `["ACTIVE","PENDING","INACTIVE"]`);

    Priority[] priorities = { Priority.LOW, Priority.HIGH };
    result = json::marshal_array(priorities);
    assert(result!! == `["LOW","HIGH"]`);

    // Test enum arrays with associated values (always uses enum names)
    State[] states = { State.WAITING, State.RUNNING, State.TERMINATED };
    result = json::marshal_array(states);
    assert(result!! == `["WAITING","RUNNING","TERMINATED"]`);  // Always uses enum names

    ComplexState[] complex_states = { ComplexState.IDLE, ComplexState.BUSY };
    result = json::marshal_array(complex_states);
    assert(result!! == `["IDLE","BUSY"]`);  // Always uses enum names
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

    String? result = json::marshal(person);
    String json = result!!;

    // Should properly escape quotes in the name
    assert(json.contains(`"name":"John \"The Coder\" Doe"`));
    assert(json.contains(`"age":25`));
    assert(json.contains(`"status":"ACTIVE"`));
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

    String? result = json::marshal(person);
    String json = result!!;

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
    
    String? result = json::marshal(data);
    String json = result!!;
    
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
    
    String? result = json::marshal(company);
    String json = result!!;
    
    // Check that nested struct is properly marshaled
    assert(json.contains(`"name":"Tech Corp"`));
    assert(json.contains(`"headquarters":{`));
    assert(json.contains(`"street":"123 Main St"`));
    assert(json.contains(`"city":"New York"`));
    assert(json.contains(`"zip_code":10001`));
    assert(json.contains(`"employee_count":500`));
    
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
    
    String? result = json::marshal(employee);
    String json = result!!;
    
    // Check deeply nested structure
    assert(json.contains(`"personal_info":{`));
    assert(json.contains(`"name":"Alice Johnson"`));
    assert(json.contains(`"age":28`));
    
    assert(json.contains(`"employer":{`));
    assert(json.contains(`"name":"Innovation Labs"`));
    assert(json.contains(`"headquarters":{`));
    assert(json.contains(`"street":"456 Tech Ave"`));
    assert(json.contains(`"city":"San Francisco"`));
    assert(json.contains(`"zip_code":94105`));
    
    assert(json.contains(`"salary":85000`));
    assert(json.contains(`"is_remote":true`));
    
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
    
    String? result = json::marshal_array(addresses);
    String json = result!!;
    
    // Check array of structs
    assert(json.starts_with("["));
    assert(json.ends_with("]"));
    assert(json.contains(`"street":"100 First St"`));
    assert(json.contains(`"city":"Boston"`));
    assert(json.contains(`"zip_code":2101`));
    assert(json.contains(`"street":"200 Second Ave"`));
    assert(json.contains(`"city":"Chicago"`));
    assert(json.contains(`"zip_code":60601`));
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
    
    String? result = json::marshal(dept);
    String json = result!!;
    
    // Check that both primitive and struct arrays are marshaled correctly
    assert(json.contains(`"name":"Engineering"`));
    assert(json.contains(`"skills_required":["C3","Rust","Go"]`));
    assert(json.contains(`"office_locations":[`));
    assert(json.contains(`"street":"100 Tech Blvd"`));
    assert(json.contains(`"city":"Austin"`));
    assert(json.contains(`"street":"200 Innovation Dr"`));
    assert(json.contains(`"city":"Seattle"`));
    assert(json.contains(`"budget":1000000`));
    assert(json.contains(`"priority":"HIGH"`));

    // Should be valid JSON format
    assert(json.starts_with("{"));
    assert(json.ends_with("}"));
}

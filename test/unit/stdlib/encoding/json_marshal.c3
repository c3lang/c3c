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

struct StatusInfo
{
    Status status;
    Priority priority;
}

struct ArrayStruct
{
    String name;
    int[3] fixed_numbers;
    String[] names;
    bool[] flags;
}

struct SliceStruct
{
    String title;
    int[] numbers;
    Person[] people;
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

// ===== UNMARSHAL TESTS =====

fn void test_simple_struct_unmarshaling() @test
{
    String json_str = `{"name":"John Doe","age":30,"is_active":true,"height":5.9,"status":"ACTIVE"}`;

    Person? result = json::unmarshal(Person, json_str, mem);
    Person person = result!!;

    assert(person.name == "John Doe");
    assert(person.age == 30);
    assert(person.is_active == true);
    assert(person.height == 5.9);
    assert(person.status == Status.ACTIVE);
}

fn void test_roundtrip_marshal_unmarshal() @test
{
    // Create original struct
    Person original = {
        .name = "Alice Smith",
        .age = 25,
        .is_active = false,
        .height = 5.6,
        .status = Status.INACTIVE
    };

    // Marshal to JSON
    String? marshal_result = json::marshal(original);
    String json = marshal_result!!;

    // Unmarshal back to struct
    Person? unmarshal_result = json::unmarshal(Person, json, mem);
    Person roundtrip = unmarshal_result!!;

    // Verify they match
    assert(roundtrip.name == original.name);
    assert(roundtrip.age == original.age);
    assert(roundtrip.is_active == original.is_active);
    assert(roundtrip.height == original.height);
    assert(roundtrip.status == original.status);
}

fn void test_nested_struct_unmarshaling() @test
{
    String json_str = `{"name":"Tech Corp","headquarters":{"street":"123 Main St","city":"New York","zip_code":10001},"employee_count":500}`;

    Company? result = json::unmarshal(Company, json_str, mem);
    Company company = result!!;

    assert(company.name == "Tech Corp");
    assert(company.headquarters.street == "123 Main St");
    assert(company.headquarters.city == "New York");
    assert(company.headquarters.zip_code == 10001);
    assert(company.employee_count == 500);
}

fn void test_missing_fields_unmarshaling() @test
{
    // Test with missing optional fields - should use default values
    String json_str = `{"name":"Partial Person","age":25}`;

    Person? result = json::unmarshal(Person, json_str, mem);
    Person person = result!!;

    assert(person.name == "Partial Person");
    assert(person.age == 25);
    // Missing fields should have default values
    assert(person.is_active == false);  // default bool value
    assert(person.height == 0.0);       // default float value
    assert(person.status == Status.ACTIVE); // default enum value (first one)
}

fn void test_array_unmarshaling() @test
{
    String json_str = `{"name":"Array Test","fixed_numbers":[1,2,3],"names":["Alice","Bob","Charlie"],"flags":[true,false,true]}`;

    ArrayStruct? result = json::unmarshal(ArrayStruct, json_str, mem);
    ArrayStruct arr_struct = result!!;

    assert(arr_struct.name == "Array Test");

    // Test fixed array
    assert(arr_struct.fixed_numbers[0] == 1);
    assert(arr_struct.fixed_numbers[1] == 2);
    assert(arr_struct.fixed_numbers[2] == 3);

    // Test string slice
    assert(arr_struct.names.len == 3);
    assert(arr_struct.names[0] == "Alice");
    assert(arr_struct.names[1] == "Bob");
    assert(arr_struct.names[2] == "Charlie");

    // Test bool slice
    assert(arr_struct.flags.len == 3);
    assert(arr_struct.flags[0] == true);
    assert(arr_struct.flags[1] == false);
    assert(arr_struct.flags[2] == true);
}

fn void test_slice_with_structs_unmarshaling() @test
{
    String json_str = `{"title":"People List","numbers":[10,20,30],"people":[{"name":"John","age":30,"is_active":true,"height":5.9,"status":"ACTIVE"},{"name":"Jane","age":25,"is_active":false,"height":5.5,"status":"INACTIVE"}]}`;

    SliceStruct? result = json::unmarshal(SliceStruct, json_str, mem);
    SliceStruct slice_struct = result!!;

    assert(slice_struct.title == "People List");

    // Test int slice
    assert(slice_struct.numbers.len == 3);
    assert(slice_struct.numbers[0] == 10);
    assert(slice_struct.numbers[1] == 20);
    assert(slice_struct.numbers[2] == 30);

    // Test struct slice
    assert(slice_struct.people.len == 2);

    assert(slice_struct.people[0].name == "John");
    assert(slice_struct.people[0].age == 30);
    assert(slice_struct.people[0].is_active == true);
    assert(slice_struct.people[0].height == 5.9);
    assert(slice_struct.people[0].status == Status.ACTIVE);

    assert(slice_struct.people[1].name == "Jane");
    assert(slice_struct.people[1].age == 25);
    assert(slice_struct.people[1].is_active == false);
    assert(slice_struct.people[1].height == 5.5);
    assert(slice_struct.people[1].status == Status.INACTIVE);
}

fn void test_array_roundtrip() @test
{
    // Create original struct with arrays
    ArrayStruct original = {
        .name = "Test Arrays",
        .fixed_numbers = {100, 200, 300},
        .names = {"First", "Second", "Third"},
        .flags = {false, true, false}
    };

    // Marshal to JSON
    String? marshal_result = json::marshal(original);
    String json = marshal_result!!;

    // Unmarshal back to struct
    ArrayStruct? unmarshal_result = json::unmarshal(ArrayStruct, json, mem);
    ArrayStruct roundtrip = unmarshal_result!!;

    // Verify they match
    assert(roundtrip.name == original.name);

    // Check fixed array
    assert(roundtrip.fixed_numbers[0] == original.fixed_numbers[0]);
    assert(roundtrip.fixed_numbers[1] == original.fixed_numbers[1]);
    assert(roundtrip.fixed_numbers[2] == original.fixed_numbers[2]);

    // Check string slice
    assert(roundtrip.names.len == original.names.len);
    assert(roundtrip.names[0] == original.names[0]);
    assert(roundtrip.names[1] == original.names[1]);
    assert(roundtrip.names[2] == original.names[2]);

    // Check bool slice
    assert(roundtrip.flags.len == original.flags.len);
    assert(roundtrip.flags[0] == original.flags[0]);
    assert(roundtrip.flags[1] == original.flags[1]);
    assert(roundtrip.flags[2] == original.flags[2]);
}





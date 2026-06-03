module interface_introspection;

import std::io;

interface JsonPrintable : Printable {}

interface Named
{
	fn String name();
}

interface PrintableNamed : Named
{
	fn void print();
}

interface Shape {}
interface Draw : Shape {}
interface Scale : Shape {}
interface Widget : Draw, Scale {}

struct JsonDocument (JsonPrintable) { int value; }

union PrintableValue (Printable) { int value; }
bitstruct PrintableFlags (Printable) : uint { uint value : 0..3; }
enum PrintableKind (Printable) { VALUE }
constdef PrintableCode (Printable) : inline uint { VALUE = 1 }
typedef PrintableId (Printable) = int;

struct UnreleatedType { int value; }

struct User (PrintableNamed) { String nombre; }
fn String User.name(&self) @dynamic => self.nombre;
fn void User.print(&self) @dynamic {}

struct Duck { bool value; }
fn String Duck.name(&self) @dynamic => "duck";

struct NamedBadReturn { bool value; }
fn int NamedBadReturn.name(&self) @dynamic => 1;

struct NamedNotDynamic { bool value; }
fn String NamedNotDynamic.name(&self) => "not dynamic";

struct PrintableName (Printable, Named) { String value; }
fn String PrintableName.name(&self) @dynamic => self.value;

struct MyWidget (Widget) { int value; }
fn void MyWidget.draw(&self) @dynamic {}
fn void MyWidget.scale(&self) @dynamic {}

struct ByteWriter { int value; }
fn sz? ByteWriter.write(&self, char[] bytes) @dynamic => bytes.len;
fn void? ByteWriter.write_byte(&self, char c) @dynamic {}

struct BytePipe { int value; }
fn sz? BytePipe.write(&self, char[] bytes) @dynamic => bytes.len;
fn void? BytePipe.write_byte(&self, char c) @dynamic {}
fn sz? BytePipe.read(&self, char[] buffer) @dynamic => buffer.len;
fn char? BytePipe.read_byte(&self) @dynamic => 'x';

fn void test_interfaces() @test
{
	assert(Printable::interfaces.len == 0);
	assert(JsonPrintable::interfaces[0] == Printable);
	assert(PrintableNamed::interfaces[0] == Named);

	assert(JsonDocument::interfaces.len == 1);
	assert(JsonDocument::interfaces[0] == JsonPrintable);
	assert(PrintableValue::interfaces[0] == Printable);
	assert(PrintableKind::interfaces[0] == Printable);
	assert(PrintableCode::interfaces[0] == Printable);
	assert(PrintableId::interfaces[0] == Printable);
	assert(PrintableFlags::interfaces[0] == Printable);

	assert(PrintableName::interfaces.len == 2);
	assert(PrintableName::interfaces[0] == Printable);
	assert(PrintableName::interfaces[1] == Named);
	
	assert(UnreleatedType::interfaces.len == 0);
}

fn void test_all_interfaces() @test
{
	assert(JsonDocument::all_interfaces.len == 2);
	assert(JsonDocument::all_interfaces[0] == JsonPrintable);
	assert(JsonDocument::all_interfaces[1] == Printable);

	assert(User::all_interfaces.len == 2);
	assert(User::all_interfaces[0] == PrintableNamed);
	assert(User::all_interfaces[1] == Named);

	assert(MyWidget::all_interfaces.len == 4);
	assert(MyWidget::all_interfaces[0] == Widget);
	assert(MyWidget::all_interfaces[1] == Draw);
	assert(MyWidget::all_interfaces[2] == Shape);
	assert(MyWidget::all_interfaces[3] == Scale);

	assert(UnreleatedType::all_interfaces.len == 0);
}

fn void test_satisfies() @test
{
	assert(User::satisfies(Named.name));
	assert(User::satisfies(PrintableNamed.print));
	assert(Duck::satisfies(Named.name));

	assert(!Duck::satisfies(PrintableNamed.print));
	assert(!NamedBadReturn::satisfies(Named.name));
	assert(!NamedNotDynamic::satisfies(Named.name));

	assert(ByteWriter::satisfies(OutStream.write));
	assert(ByteWriter::satisfies(OutStream.write_byte));
	assert(!ByteWriter::satisfies(InStream.read));
	assert(BytePipe::satisfies(InStream.read));

	assert(!UnreleatedType::satisfies(Named.name));
}


import std::text::i18n;
import std::io::path;

fn void translate_from_json() @test {
    Allocator allocator = allocator::heap();
	Translator translator = i18n::new_translator(allocator);
	defer translator.free();

	String cwd = path::cwd(allocator)!!.dirname();
	defer cwd.free(allocator);

	String file_path = string::format(allocator, "%s%s", cwd, "/test/unit/stdlib/text/i18n/i18n_en.json");
	defer file_path.free(allocator);
	
	translator.load_from_json(file_path, "en")!!;

	translator.set_language("en")!!;
	assert(translator.currentLanguage, "en");

	String greeting = translator.tr("greeting", "C3")!!;
	defer greeting.free(allocator); 
	assert(greeting, "Hello, C3!");

	String farewell = translator.tr("farewell", "C3")!!;
	defer farewell.free(allocator); 
	assert(farewell, "Goodbye, C3!");
}
import std::text::i18n;

fn void translate_from_json() @test {
    Allocator allocator = allocator::heap();
	Translator translator = new_translator(allocator);

	translator.load_from_json("./i18n_en.json", "en")!!;

	translator.set_language("en")!!;
	assert(translator.currentLanguage, "en");

	String greeting = translator.tr("greeting", "C3")!!;
	assert(greeting, "Hello, C3!");

	String farewell = translator.tr("farewell", "C3")!!;
	assert(farewell, "Goodbye, C3!");
}
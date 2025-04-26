import std::text::i18n;

fn void translate_from_json() @test {
    Allocator allocator = allocator::heap();
	Translator translator = new_translator(allocator);

	if (catch excuse = translator.load_from_json("./i18n_en.json", "en")) {
		io::printfn("Error loading JSON: %s\n", excuse);
	}

	translator.set_language("en")!!;

	io::printfn("Current Language: %s", translator.currentLanguage);

	String greeting = translator.tr("greeting", "C3")!!;
	io::printfn("- Greeting: %s", greeting);

	String farewell = translator.tr("farewell", "C3")!!;
	io::printfn("- Farewell: %s", farewell);
}
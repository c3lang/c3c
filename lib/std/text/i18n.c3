module std::text::i18n;
import std::io;
import std::io::file;
import std::os;
import std::core::mem::allocator;
import std::collections::map;
import std::collections::object;
import std::encoding::json;
import std::time;

alias LanguageMap = HashMap{String, Object*};
alias TranslationMap = HashMap{String, LanguageMap};

// This fault raised when translation key not found.
faultdef NOT_FOUND;

struct Translator (Printable) {
	Allocator allocator;
	TranslationMap tmap;
	String currentLanguage;
}

fn Translator new_translator(Allocator allocator, String currentLanguage = "en") {
	Translator t;
	t.allocator = allocator;
	t.tmap.init(allocator);
	t.currentLanguage = currentLanguage;
	return t;
}

fn void? Translator.load_from_json(&self, String file_path, String code) {
	String file_content = (String) file::load(self.allocator, file_path)!;
	Object* object = json::parse_string(self.allocator, file_content)!;

	if (self.tmap.has_key(code)) {
		return io::ALREADY_EXISTS?;
	}

	self.tmap.set(code, object.map);
}

fn void? Translator.set_language(&self, String code) {
	if (!self.tmap.has_key(code)) {
		return io::FILE_NOT_FOUND?;
	}
	
	self.currentLanguage = code;
}

macro Translator.tr(&self, key, ...) {
	if (try LanguageMap lmap = self.tmap.get(self.currentLanguage)) {
		Object* value = lmap.get(key)!;

		if (!value.is_string()) {
			return io::UNSUPPORTED_OPERATION?;
		}
																
		return string::format(self.allocator, value.s, $vasplat[..]);
	}

	return NOT_FOUND?;
}

// TODO 
// Implement Translator.load to load all .json files from directory as LanguageMap.
// This requires iterating over the directory and loading each file.

// TODO 
// Implement pluralization support.

// TODO 
// Implement load from yaml.
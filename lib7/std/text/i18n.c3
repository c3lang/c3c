/*module std::text::i18n;
import std::collections::map;
import std::hash::fnv32a;
distinct Language = char[];

const Language EN = "en";

def TranslationMap = HashMap{String, String};
fn uint Language.hash(self) => fnv32a::encode((char[])self);
HashMap{Language, TranslationMap*} language_map @private;
TranslationMap! current_map;

macro String @localized(String string) @builtin
{
	return current_map[string] ?? string;
}


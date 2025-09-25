module std::crypto::aes128;
import std::crypto::aes;

fn char[] encrypt(Allocator allocator, char[16]* key, char[aes::BLOCKLEN] iv, char[] data)
{
	Aes aes @noinit;
	aes.init_with_iv(AES128, CTR, key, iv);
	defer aes.destroy();
	return aes.encrypt(allocator, data);
}

fn char[] tencrypt(char[16]* key, char[aes::BLOCKLEN] iv, char[] data)
{
	return encrypt(tmem, key, iv, data);
}

fn char[] decrypt(Allocator allocator, char[16]* key, char[aes::BLOCKLEN] iv, char[] data)
{
	Aes aes @noinit;
	aes.init_with_iv(AES128, CTR, key, iv);
	defer aes.destroy();
	return aes.decrypt(allocator, data);
}

fn char[] tdecrypt(char[16]* key, char[aes::BLOCKLEN] iv, char[] data)
{
	return decrypt(tmem, key, iv, data);
}

module std::crypto::aes192;
import std::crypto::aes;

fn char[] encrypt(Allocator allocator, char[24]* key, char[aes::BLOCKLEN] iv, char[] data)
{
	Aes aes @noinit;
	aes.init_with_iv(AES192, CTR, key, iv);
	defer aes.destroy();
	return aes.encrypt(allocator, data);
}

fn char[] tencrypt(char[24]* key, char[aes::BLOCKLEN] iv, char[] data)
{
	return encrypt(tmem, key, iv, data);
}

fn char[] decrypt(Allocator allocator, char[24]* key, char[aes::BLOCKLEN] iv, char[] data)
{
	Aes aes @noinit;
	aes.init_with_iv(AES192, CTR, key, iv);
	defer aes.destroy();
	return aes.decrypt(allocator, data);
}

fn char[] tdecrypt(char[24]* key, char[aes::BLOCKLEN] iv, char[] data)
{
	return decrypt(tmem, key, iv, data);
}

module std::crypto::aes256;
import std::crypto::aes;

fn char[] encrypt(Allocator allocator, char[32]* key, char[aes::BLOCKLEN] iv, char[] data)
{
	Aes aes @noinit;
	aes.init_with_iv(AES256, CTR, key, iv);
	defer aes.destroy();
	return aes.encrypt(allocator, data);
}

fn char[] tencrypt(char[32]* key, char[aes::BLOCKLEN] iv, char[] data)
{
	return encrypt(tmem, key, iv, data);
}

fn char[] decrypt(Allocator allocator, char[32]* key, char[aes::BLOCKLEN] iv, char[] data)
{
	Aes aes @noinit;
	aes.init_with_iv(AES256, CTR, key, iv);
	defer aes.destroy();
	return aes.decrypt(allocator, data);
}

fn char[] tdecrypt(char[32]* key, char[aes::BLOCKLEN] iv, char[] data)
{
	return decrypt(tmem, key, iv, data);
}

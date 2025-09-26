// Experimental implementation
module std::crypto::aes128;
import std::crypto::aes;

fn char[] encrypt(Allocator allocator, char[16]* key, char[aes::BLOCKLEN] iv, char[] data)
{
	Aes aes @noinit;
	aes.init(AES128, key, iv, CTR);
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
	aes.init(AES128, key, iv, CTR);
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
	aes.init(AES192, key, iv, CTR);
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
	aes.init(AES192, key, iv, CTR);
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
	aes.init(AES256, key, iv, CTR);
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
	aes.init(AES256, key, iv, CTR);
	defer aes.destroy();
	return aes.decrypt(allocator, data);
}

fn char[] tdecrypt(char[32]* key, char[aes::BLOCKLEN] iv, char[] data)
{
	return decrypt(tmem, key, iv, data);
}

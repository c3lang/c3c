module std::crypto::aes_bench;

import std::crypto::aes;

fn void init() @init
{
	set_benchmark_warmup_iterations(5);
	set_benchmark_max_iterations(10_000);
}


AesType aes   = AES256;
char[] key    = x"603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4";
char[] text   = x"6bc1bee22e409f96e93d7e117393172aae2d8a571e03ac9c9eb76fac45af8e5130c81c46a35ce411e5fbc1191a0a52eff69f2445df4f9b17ad2b417be66c3710";
char[] cipher = x"601ec313775789a5b7a7f504bbf3d228f443e3ca4d62b59aca84e990cacaf5c52b0930daa23de94ce87017ba2d84988ddfc9c58db67aada613c2dd08457941a6";
char[16] iv     = x"f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff";

fn void bench_ctr_xcrypt() @benchmark
{
	char[64] out;
	Aes ctx;

	// encrypt
	ctx.init_with_iv(aes, CTR, key, iv);
	ctx.encrypt_buffer(text, &out);

	// decrypt
	ctx.init_with_iv(aes, CTR, key, iv);
	ctx.decrypt_buffer(cipher, &out);
}


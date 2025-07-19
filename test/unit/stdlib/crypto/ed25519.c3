module std::crypto::ed25519_test @test;
import std::crypto::ed25519;

fn void ed25519_keygen_sign_verify() @test
{
	Ed25519PrivateKey private_key = x"97b0d0435be3a583df0b77aff277e5fc8f857cae1a827723241bb9bbf7e96018";

	Ed25519PublicKey public_key = ed25519::public_keygen(&private_key);
	assert(public_key == x"fd462c5c7044b224f4a6cc0f0e5e16511be881be639b3fbc600f7b5d6da54e2f");

	String message = "abcdefghijklmnopqrstuvwxyz0123456789";

	Ed25519Signature signature = ed25519::sign(message, &private_key, &public_key);
	assert(signature == x"c03cf7b181583f6353a9434be910a484e1720247a0dc384d601e495294d0202728dda5f31bc62b00158b1588bc2a65f48ccfaa1b1431aff3d8da9271b7de9403");
	assert(ed25519::verify(message, &signature, &public_key));
}

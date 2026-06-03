// Regression test: load two independently built C3 dynamic libraries into the
// same (non-C3) host via dlopen. Each .dylib/.so carries its own C3 runtime, so
// the second one's startup must initialise only its own image. Before that fix,
// loading the second C3 library crashed on macOS in the dyld add-image hook.
#include <stdio.h>
#include <dlfcn.h>

typedef int (*adder_fn)(int, int);

static void *load(const char *path)
{
	void *h = dlopen(path, RTLD_NOW | RTLD_LOCAL);
	if (!h) fprintf(stderr, "dlopen %s failed: %s\n", path, dlerror());
	return h;
}

int main(void)
{
#if defined(__APPLE__)
	const char *p1 = "./add.dylib", *p2 = "./add2.dylib";
#else
	const char *p1 = "./add.so", *p2 = "./add2.so";
#endif

	void *h1 = load(p1);
	if (!h1) return 1;
	adder_fn adder = (adder_fn)dlsym(h1, "adder");

	// The second C3 library is the one that used to crash on load.
	void *h2 = load(p2);
	if (!h2) return 1;
	adder_fn adder2 = (adder_fn)dlsym(h2, "adder2");

	if (!adder || !adder2) return 1;
	if (adder(1, 4) != 5 || adder2(10, 20) != 30) return 1;
	return 0;
}

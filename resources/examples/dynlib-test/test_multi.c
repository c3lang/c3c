// Regression test: dlopen two independently built C3 dynamic libraries into one
// non-C3 host. Each carries its own C3 runtime, so the second must initialise
// only its own image -- this used to crash on macOS in the dyld add-image hook.
//
// Also checks @init priority + side-effects, @dynamic dispatch, and @finalizer
// execution. The workload runs in a forked child so the parent can read its full
// stdout, including output from C3 finalizers during exit().
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <dlfcn.h>
#include <sys/wait.h>

typedef int (*adder_fn)(int, int);
typedef int (*int_fn)(void);
typedef char *(*str_fn)(void);

static void *load(const char *path)
{
	void *h = dlopen(path, RTLD_NOW | RTLD_LOCAL);
	if (!h) fprintf(stderr, "dlopen %s failed: %s\n", path, dlerror());
	return h;
}

static int workload(void)
{
#if defined(__APPLE__)
	const char *p1 = "./add.dylib", *p2 = "./add2.dylib";
#else
	const char *p1 = "./add.so", *p2 = "./add2.so";
#endif

	void *h1 = load(p1);
	if (!h1) return 1;
	adder_fn adder  = (adder_fn)dlsym(h1, "adder");
	int_fn   ivalue = (int_fn)dlsym(h1, "add_init_value");
	str_fn   iorder = (str_fn)dlsym(h1, "add_init_order");

	// The second C3 library is the one that used to crash on load.
	void *h2 = load(p2);
	if (!h2) return 1;
	adder_fn adder2 = (adder_fn)dlsym(h2, "adder2");

	if (!adder || !adder2 || !ivalue || !iorder) {
		fprintf(stderr, "missing symbol\n");
		return 1;
	}

	if (adder(1, 4) != 5 || adder2(10, 20) != 30) {
		fprintf(stderr, "wrong result\n");
		return 1;
	}

	if (ivalue() != 42) {
		fprintf(stderr, "init_value = %d, expected 42\n", ivalue());
		return 1;
	}

	// @init(100) runs before @init(200) => "AB".
	if (strcmp(iorder(), "AB") != 0) {
		fprintf(stderr, "init_order = \"%s\", expected \"AB\"\n", iorder());
		return 1;
	}

	return 0;
}

int main(void)
{
	int fds[2];
	if (pipe(fds) != 0) { perror("pipe"); return 1; }

	pid_t pid = fork();
	if (pid < 0) { perror("fork"); return 1; }

	if (pid == 0) {
		// Exit normally so C3 finalizers run and their output reaches the pipe.
		close(fds[0]);
		dup2(fds[1], STDOUT_FILENO);
		close(fds[1]);
		exit(workload());
	}

	// Parent: drain the child's stdout, then check exit status + markers.
	close(fds[1]);
	char buf[4096];
	size_t total = 0;
	ssize_t n;
	while (total < sizeof(buf) - 1 &&
	       (n = read(fds[0], buf + total, sizeof(buf) - 1 - total)) > 0) {
		total += (size_t)n;
	}
	buf[total] = '\0';
	close(fds[0]);

	int status = 0;
	waitpid(pid, &status, 0);

	fputs(buf, stdout); // surface the child's output for CI logs

	if (!WIFEXITED(status) || WEXITSTATUS(status) != 0) {
		fprintf(stderr, "workload failed (status %d)\n", status);
		return 1;
	}
	if (!strstr(buf, "Bar[10]")) {
		fprintf(stderr, "add2 @dynamic dispatch missing\n");
		return 1;
	}
	if (!strstr(buf, "add: finalizer ran")) {
		fprintf(stderr, "finalizer did not run\n");
		return 1;
	}
	return 0;
}

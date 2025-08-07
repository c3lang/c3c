module std::os::linux @test @if (env::LINUX);
import std::net::os;
import std::os::posix;
import libc;

fn void test_epoll_create()
{
	// Test epoll_create with valid size
	int epoll_fd = epoll_create(1);
	assert(epoll_fd >= 0, "epoll_create failed");
	libc::close(epoll_fd);

	// Test epoll_create with size 0 (should work on Linux)
	epoll_fd = epoll_create1(0);
	assert(epoll_fd >= 0, "epoll_create1 failed");
	libc::close(epoll_fd);

	// Test epoll_create with negative size (should fail)
	epoll_fd = epoll_create(-1);
	assert(epoll_fd == -1, "epoll_create with negative size should fail");
}

fn void test_epoll_ctl()
{
	int epoll_fd = epoll_create(1);
	assert(epoll_fd >= 0, "epoll_create failed");

	// Create a socket to monitor
	int sock_fd = os::socket(os::AF_INET, os::SOCK_STREAM, 0);
	assert(sock_fd >= 0, "socket creation failed");

	EpollEvent event;
	event.events = EPOLLIN;
	event.data.fd = sock_fd;

	// Test EPOLL_CTL_ADD
	int ret = epoll_ctl(epoll_fd, EPOLL_CTL_ADD, sock_fd, &event);
	assert(ret == 0, "EPOLL_CTL_ADD failed");

	// Test EPOLL_CTL_MOD
	event.events = EPOLLIN | EPOLLOUT;
	ret = epoll_ctl(epoll_fd, EPOLL_CTL_MOD, sock_fd, &event);
	assert(ret == 0, "EPOLL_CTL_MOD failed");

	// Test EPOLL_CTL_DEL
	ret = epoll_ctl(epoll_fd, EPOLL_CTL_DEL, sock_fd, null);
	assert(ret == 0, "EPOLL_CTL_DEL failed");

	// Test invalid operations
	ret = epoll_ctl(epoll_fd, -1, sock_fd, &event); // invalid op
	assert(ret == -1, "Invalid operation should fail");

	libc::close(sock_fd);
	libc::close(epoll_fd);
}

fn void test_epoll_edge_triggered()
{
	int epoll_fd = epoll_create(1);
	assert(epoll_fd >= 0, "epoll_create failed");

	int[2] pipe_fds;
	assert(posix::pipe(&pipe_fds) == 0, "pipe creation failed");

	// Set up edge-triggered monitoring
	EpollEvent event;
	event.events = EPOLLIN | EPOLLET;
	event.data.fd = pipe_fds[0];
	assert(epoll_ctl(epoll_fd, EPOLL_CTL_ADD, pipe_fds[0], &event) == 0,
		   "epoll_ctl add failed");

	// Write to pipe
	char[] buf = "test";
	assert(libc::write(pipe_fds[1], buf, buf.len) == buf.len,
		   "write to pipe failed");

	// First epoll_wait should get the event
	EpollEvent[10] events;
	int num_events = epoll_wait(epoll_fd, &events[0], events.len, 0);
	assert(num_events == 1, "epoll_wait should return 1 event");

	// Second epoll_wait shouldn't get the same event (edge-triggered behavior)
	num_events = epoll_wait(epoll_fd, &events[0], events.len, 0);
	assert(num_events == 0, "epoll_wait should not return event again for edge-triggered");

	libc::close(pipe_fds[0]);
	libc::close(pipe_fds[1]);
	libc::close(epoll_fd);
}

fn void test_epoll_level_triggered()
{
	int epoll_fd = epoll_create(1);
	assert(epoll_fd >= 0, "epoll_create failed");

	int[2] pipe_fds;
	assert(posix::pipe(&pipe_fds) == 0, "pipe creation failed");

	// Set up level-triggered monitoring
	EpollEvent event;
	event.events = EPOLLIN; // Default is level-triggered
	event.data.fd = pipe_fds[0];
	assert(epoll_ctl(epoll_fd, EPOLL_CTL_ADD, pipe_fds[0], &event) == 0,
		   "epoll_ctl add failed");

	// Write to pipe
	char[] buf = "test";
	assert(libc::write(pipe_fds[1], buf, buf.len) == buf.len,
		   "write to pipe failed");

	// First epoll_wait should get the event
	EpollEvent[10] events;
	int num_events = epoll_wait(epoll_fd, &events[0], events.len, 0);
	assert(num_events == 1, "epoll_wait should return 1 event");

	// Second epoll_wait should get the same event again (level-triggered behavior)
	num_events = epoll_wait(epoll_fd, &events[0], events.len, 0);
	assert(num_events == 1, "epoll_wait should return event again for level-triggered");

	libc::close(pipe_fds[0]);
	libc::close(pipe_fds[1]);
	libc::close(epoll_fd);
}

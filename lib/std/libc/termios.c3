module libc::termios;

fn int send_break(Fd fd, int duration) => tcsendbreak(fd, duration);
fn int drain(Fd fd) => tcdrain(fd);
fn int flush(Fd fd, int queue_selector) => tcflush(fd, queue_selector);
fn int flow(Fd fd, int action) => tcflow(fd, action);
fn Speed Termios.get_ospeed(&self) => cfgetospeed(self);
fn Speed Termios.get_ispeed(&self) => cfgetispeed(self);
fn int Termios.set_ospeed(&self, Speed speed) => cfsetospeed(self, speed);
fn int Termios.set_ispeed(&self, Speed speed) => cfsetispeed(self, speed);
fn int Termios.get_attr(&self, Fd fd) => tcgetattr(fd, self);
fn int Termios.set_attr(&self, Fd fd, Tcactions optional_actions) => tcsetattr(fd, optional_actions, self);

module libc::termios @if(!env::LIBC ||| !env::POSIX);

typedef Speed = CUInt;
typedef Tcactions = CInt;

struct Termios
{
	void* dummy;
}

fn CInt tcgetattr(Fd fd, Termios* self)
{
	unreachable("tcgetattr unavailable");
}

fn CInt tcsetattr(Fd fd, Tcactions optional_actions, Termios* self)
{
	unreachable("tcsetattr unavailable");
}

fn CInt tcsendbreak(Fd fd, CInt duration)
{
	unreachable("tcsendbreak unavailable");
}

fn CInt tcdrain(Fd fd)
{
	unreachable("tcdrain unavailable");
}

fn CInt tcflush(Fd fd, CInt queue_selector)
{
	unreachable("tcflush unavailable");
}

fn CInt tcflow(Fd fd, CInt action)
{
    unreachable("tcflow unavailable");
}

fn Speed cfgetospeed(Termios* self)
{
	unreachable("cfgetospeed unavailable");
}

fn Speed cfgetispeed(Termios* self)
{
	unreachable("cfgetispeed unavailable");
}

fn CInt cfsetospeed(Termios* self, Speed speed)
{
	unreachable("cfsetospeed unavailable");
}

fn CInt cfsetispeed(Termios* self, Speed speed)
{
	unreachable("cfsetispeed unavailable");
}

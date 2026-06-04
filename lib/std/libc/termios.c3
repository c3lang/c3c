module libc::termios @if(env::LIBC &&& (env::LINUX ||| env::DARWIN));

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

module my::base::log;
fn void info() {}

module my::thing;
import my::base;

fn void main()
{
	log::info();
}
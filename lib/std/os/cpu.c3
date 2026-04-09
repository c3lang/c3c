// https://www.cprogramming.com/snippets/source-code/find-the-number-of-cpu-cores-for-windows-mac-or-linux

module std::os @if(env::DARWIN);
import std::os::macos;

fn int num_cpu()
{
	int[2] nm;
	usz len = 4;
	uint count;

	nm = { darwin::CTL_HW, darwin::HW_NCPU };
	darwin::sysctl(&nm, 2, &count, &len, null, 0);
	if (count < 1) count = 1;
	return (int)count;
}

module std::os @if(env::LINUX || env::ANDROID);
import std::os::posix;

fn int num_cpu()
{
	return (int)posix::get_nprocs_conf();
}

module std::os @if(env::WIN32);
import std::os::win32;

fn int num_cpu()
{
	Win32_SYSTEM_INFO info;
	win32::getSystemInfo(&info);
	return (int)info.dwNumberOfProcessors;
}

module std::os @if(env::NETBSD);
import std::os::netbsd;
import libc;

fn uint num_cpu()
{
	int[2] nm;
	usz len = 4;
	uint count;

	nm = { netbsd::CTL_HW, netbsd::HW_NCPU };
	libc::sysctl(&nm, 2, &count, &len, null, 0);
	if (count < 1) count = 1;
	return count;
}

module std::os @if(env::OPENBSD);
import std::os::openbsd;
import libc;

fn int num_cpu()
{
	int[2] nm;
	usz len = 4;
	uint count;

	nm = { openbsd::CTL_HW, openbsd::HW_NCPUONLINE };
	if (libc::sysctl(&nm, 2, &count, &len, null, 0) == 0 && count >= 1)
	{
		return count;
	}

	nm = { openbsd::CTL_HW, openbsd::HW_NCPU };
	libc::sysctl(&nm, 2, &count, &len, null, 0);
	if (count < 1) count = 1;
	return (int)count;
}

module std::os @if(env::FREEBSD);
import std::os::freebsd;
import libc;

fn int num_cpu()
{
	int[2] nm;
	usz len = 4;
	uint count;

	nm = { freebsd::CTL_HW, freebsd::HW_NCPU };
	libc::sysctl(&nm, 2, &count, &len, null, 0);
	if (count < 1) count = 1;
	return (int)count;
}

module std::os @if(env::EMSCRIPTEN);
import libc;

fn uint num_cpu()
{
	uint count = (uint)libc::emscripten_num_logical_cores();
	if (count < 1) count = 1;
	return count;
}

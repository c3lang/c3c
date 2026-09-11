module std::gfx @feat(X11_WAYLAND);


import std::math, libc, std::net::os, std::os::posix, std::time;


// public
fn void todo_fix_stopCheckEvents()
{
	_gfx_ctx.eventWait_forceStop[2] = 1;
	while (true)
	{
		char byte = 0;
		sz result = libc::write(_gfx_ctx.eventWait_forceStop[1], &byte, 1);
		if (result == 1 || result == -1) break;
	}
}

// public
fn void todo_fix_waitForEvent(int waitMS)
{
	if (waitMS == 0) return;

	if (_gfx_ctx.eventWait_forceStop[0] == 0 || _gfx_ctx.eventWait_forceStop[1] == 0)
	{
		if (posix::pipe((int[2]*)&_gfx_ctx.eventWait_forceStop) != -1)
		{
			os::fcntl((NativeSocket)_gfx_ctx.eventWait_forceStop[0], os::F_GETFL, 0);
			os::fcntl((NativeSocket)_gfx_ctx.eventWait_forceStop[0], os::F_GETFD, 0);
			os::fcntl((NativeSocket)_gfx_ctx.eventWait_forceStop[1], os::F_GETFL, 0);
			os::fcntl((NativeSocket)_gfx_ctx.eventWait_forceStop[1], os::F_GETFD, 0);
		}
	}

	Posix_pollfd[2] fds;
	fds[0].fd = 0;
	fds[0].events = (short)os::POLLIN;
	fds[1].fd = _gfx_ctx.eventWait_forceStop[0];
	fds[1].events = (short)os::POLLIN;

	if (gfx_use_wayland)
	{
		fds[0].fd = wlDisplayGetFd(_gfx_ctx.wlDisplay);

		// empty the queue
		while (wlDisplayPrepareRead(_gfx_ctx.wlDisplay) != 0)
		{
			// error occured when dispatching the queue
			if (wlDisplayDispatchPending(_gfx_ctx.wlDisplay) == -1) return;
		}

		// send any pending requests to the compositor
		while (wlDisplayFlush(_gfx_ctx.wlDisplay) == -1)
		{
			// queue is full dispatch them
			if (libc::errno() == EAGAIN)
			{
				if (wlDisplayDispatchPending(_gfx_ctx.wlDisplay) == -1) return;
			}
			else { return; }
		}
	}
	else
	{
		fds[0].fd = xConnectionNumber(_gfx_ctx.display);
	}

	Clock current = clock::now();
	if (gfx_use_wayland)
	{
		while (wlDisplayDispatchPending(_gfx_ctx.wlDisplay) == 0)
		{
			if (os::poll(&fds[0], 1, waitMS) <= 0)
			{
				wlDisplayCancelRead(_gfx_ctx.wlDisplay);
				break;
			}
			else
			{
				if (wlDisplayReadEvents(_gfx_ctx.wlDisplay) == -1) return;
			}

			if (waitMS != TODO_RENAME_EVENT_WAIT_NEXT)
			{
				waitMS -= (int)current.to_now().to_duration().to_ms();
			}
		}

		// queue contains events from read, dispatch them
		if (wlDisplayDispatchPending(_gfx_ctx.wlDisplay) == -1) return;
	}
	else
	{
		while (xPending(_gfx_ctx.display) == 0)
		{
			if (os::poll(&fds[0], 1, waitMS) <= 0) break;

			if (waitMS != TODO_RENAME_EVENT_WAIT_NEXT)
			{
				waitMS -= (int)current.to_now().to_duration().to_ms();
			}
		}
	}

	// drain any data in the stop request
	if (_gfx_ctx.eventWait_forceStop[2])
	{
		char[64] data;
		libc::read(_gfx_ctx.eventWait_forceStop[0], &data, 64);

		_gfx_ctx.eventWait_forceStop[2] = 0;
	}
}

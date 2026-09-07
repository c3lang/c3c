module std::gfx @feat(WIN32) @private;

fn void rgfw_window_showPlatform(GfxWindow* win)
{
	if (!win.internal.flags.maximize) showWindow(win.src.window, (int)SW_RESTORE);
}

module std::gfx @feat(WIN32) @link("user32", "gdi32", "shell32", "advapi32", "dwmapi", "opengl32", "winmm");

module std::gfx @feat(WIN32);
import std::collections::list, std::math, std::os::win32;

fn void todo_fix_monitorNode_free(GfxMonitorNode* node)
{}

// public

alias GetExtStringArbFn = fn ZString(Win32_HDC*);
alias GetExtStringExtFn = fn ZString();
alias GlxGetProcAddressProc = fn GfxFn(ZString);



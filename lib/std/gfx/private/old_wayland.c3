module std::gfx @feat(WAYLAND) @link("wayland-client", "wayland-cursor", "wayland-egl", "xkbcommon", "EGL");
import std::collections::list, std::math, libc;



// ---- Wayland client core (real libwayland-client ABI) ----

const uint WL_MARSHAL_FLAG_DESTROY = 1;


// private
fn void rgfwDoNothing() {}


// public

struct TodoRenameAttribStack
{
	int* attribs;
	sz count;
	sz max;
}







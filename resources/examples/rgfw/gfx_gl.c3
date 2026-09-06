module std::gfx @feat(!WASM);

fn void rgfw_attribStack_init(RgfwAttribStack* stack, int* attribs, sz max) { stack.attribs = attribs; stack.count = 0; stack.max = max; }
fn void rgfw_attribStack_pushAttrib(RgfwAttribStack* stack, int attrib) { stack.attribs[stack.count] = attrib; stack.count += 1; }
fn void rgfw_attribStack_pushAttribs(RgfwAttribStack* stack, int attrib1, int attrib2) { rgfw_attribStack_pushAttrib(stack, attrib1); rgfw_attribStack_pushAttrib(stack, attrib2); }

fn void unload_gl()
{
	_rgfw.nativeGL_handle = null;
	_rgfw.glGetProcAddress = null;
}

module std::gfx @feat(WASM);

fn void unload_gl()
{}

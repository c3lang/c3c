module metal_window;
import std::gfx, std::os::darwin, std::os::macos, std::os::macos::cg, std::time;

// Metal in a std::gfx window on macOS. One full screen fragment shader draws the whole scene: a red frame around the window, a green
// box top left, a blue box bottom right, a white cross and a line that sweeps across the window. Resize the window to see that the frame
// is drawn together with the new window size. F toggles fullscreen, ESC quits.

const String SHADER = `
#include <metal_stdlib>
using namespace metal;
struct V { float4 pos [[position]]; };
struct U { float2 size; float time; float pad; };
vertex V scene_vertex(uint vid [[vertex_id]])
{
	V o;
	o.pos = float4((vid & 1) ? 1.0 : -1.0, (vid & 2) ? 1.0 : -1.0, 0.0, 1.0);
	return o;
}
fragment float4 scene_fragment(V in [[stage_in]], constant U& u [[buffer(0)]])
{
	float2 p = in.pos.xy;   // in pixels, the origin is top left
	float2 s = u.size;
	float3 c = mix(float3(0.05, 0.06, 0.10), float3(0.10, 0.13, 0.22), p.y / s.y);
	float d = abs(p.x - fract(u.time / 4.0) * s.x);
	c += float3(0.35) * smoothstep(24.0, 0.0, d);
	if (d < 4.0) c = float3(1.0);
	if (abs(p.x - s.x * 0.5) < 2.0 && abs(p.y - s.y * 0.5) < 60.0) c = float3(1.0);
	if (abs(p.y - s.y * 0.5) < 2.0 && abs(p.x - s.x * 0.5) < 60.0) c = float3(1.0);
	if (p.x >= 4.0 && p.y >= 4.0 && p.x < 84.0 && p.y < 84.0) c = float3(0.0, 1.0, 0.0);
	if (p.x >= s.x - 84.0 && p.y >= s.y - 84.0 && p.x < s.x - 4.0 && p.y < s.y - 4.0) c = float3(0.0, 0.0, 1.0);
	if (p.x < 4.0 || p.y < 4.0 || p.x >= s.x - 4.0 || p.y >= s.y - 4.0) c = float3(1.0, 0.0, 0.0);
	return float4(c, 1.0);
}
`;

struct Uniforms
{
	float[2] size;
	float time;
	float pad;
}

ObjId layer;
ObjId queue;
ObjId pipeline;
double start_time;

// Draws a frame. gfx calls this while the window is resized, from inside the transaction that resizes the layer, and the loop calls it to animate.
fn void draw(GfxWindow* win, GfxRect rect) => objc::@autoreleasepool()
{
	CGSize size = layer.send(CGSize, "drawableSize");   // gfx keeps it at the size of the window in pixels
	ObjId drawable = layer.send(ObjId, "nextDrawable");
	if (!drawable) return;
	ObjId pass = objc::@class("MTLRenderPassDescriptor").send(ObjId, "renderPassDescriptor");
	ObjId attachment = pass.send(ObjId, "colorAttachments").send(ObjId, "objectAtIndexedSubscript:", (NSUInteger)0);
	attachment.send_void("setTexture:", drawable.send(ObjId, "texture"));
	attachment.send_void("setLoadAction:", (NSUInteger)0);   // don't care, every pixel is drawn
	attachment.send_void("setStoreAction:", (NSUInteger)1);
	ObjId commands = queue.send(ObjId, "commandBuffer");
	ObjId encoder = commands.send(ObjId, "renderCommandEncoderWithDescriptor:", pass);
	Uniforms uniforms = { { (float)size.width, (float)size.height }, (float)(time::now().to_seconds() - start_time), 0 };
	encoder.send_void("setRenderPipelineState:", pipeline);
	encoder.send_void("setFragmentBytes:length:atIndex:", &uniforms, (NSUInteger)16, (NSUInteger)0);
	encoder.send_void("drawPrimitives:vertexStart:vertexCount:", (NSUInteger)4, (NSUInteger)0, (NSUInteger)4);   // triangle strip
	encoder.send_void("endEncoding");
	// The layer presents with the transaction: commit, wait until the frame is scheduled, then present.
	commands.send_void("commit");
	commands.send_void("waitUntilScheduled");
	drawable.send_void("present");
}

fn int main()
{
	gfx::init("MetalWindow", {})!!;
	defer gfx::finalize();
	GfxWindow* win = gfx::create_window("gfx metal", 200, 200, 900, 600, {})!!;
	defer win.destroy();

	layer = (ObjId)win.create_metal_layer()!!;
	ObjId device = layer.send(ObjId, "device");
	queue = device.send(ObjId, "newCommandQueue");
	ObjId library = device.send(ObjId, "newLibraryWithSource:options:error:", SHADER.to_nsstring(), null, null);
	ObjId descriptor = objc::@class("MTLRenderPipelineDescriptor").alloc().send(ObjId, "init");
	descriptor.send_void("setVertexFunction:", library.send(ObjId, "newFunctionWithName:", "scene_vertex".to_nsstring()));
	descriptor.send_void("setFragmentFunction:", library.send(ObjId, "newFunctionWithName:", "scene_fragment".to_nsstring()));
	descriptor.send(ObjId, "colorAttachments").send(ObjId, "objectAtIndexedSubscript:", (NSUInteger)0).send_void("setPixelFormat:", MTLPixelFormat.BGRA8_UNORM);
	pipeline = device.send(ObjId, "newRenderPipelineStateWithDescriptor:error:", descriptor, null);

	win.set_repaint_callback(&draw);
	start_time = time::now().to_seconds();
	bool quit = false;
	while (!quit)
	{
		if (try e = gfx::wait(8 * time::MS))
		{
			switch (e.type)
			{
				case WINDOW_CLOSE: quit = true;
				case KEY_PRESSED:
					if (e.key.value == ESCAPE) quit = true;
					if (e.key.value == F) win.set_fullscreen(!win.is_fullscreen());
				default: break;
			}
		}
		draw(win, {});
	}
	return 0;
}

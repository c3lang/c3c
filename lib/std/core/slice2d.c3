module std::core::array::slice {Type};

<*
 A slice2d allows slicing an array like int[10][10] into an arbitrary "int[][]"-like counterpart
 Typically you'd use array::slice2d(...) to create one.
*>
struct Slice2d
{
	Type* ptr;
	usz inner_len;
	usz ystart;
	usz ylen;
	usz xstart;
	usz xlen;
}

<*
 @return `The length of the "outer" slice`
*>
fn usz Slice2d.len(&self) @operator(len)
{
	return self.ylen;
}

<*
 @return `The total number of elements.`
*>
fn usz Slice2d.count(&self)
{
	return self.ylen * self.xlen;
}

<*
 Step through each element of the slice.
*>
macro void Slice2d.@each(&self; @body(usz[<2>], Type))
{
	foreach (y, line : *self)
	{
		foreach (x, val : line)
		{
			@body({ x, y }, val);
		}
	}
}

<*
 Step through each element of the slice *by reference*
*>
macro void Slice2d.@each_ref(&self; @body(usz[<2>], Type*))
{
	foreach (y, line : *self)
	{
		foreach (x, &val : line)
		{
			@body({ x, y }, val);
		}
	}
}

<*
 Return a row as a slice.

 @param idy : "The row to return"
 @return "The slice for the particular row"
 @require idy >= 0 && idy < self.ylen
*>
macro Type[] Slice2d.get_row(self, usz idy) @operator([])
{
	return (self.ptr + self.inner_len * (idy + self.ystart))[self.xstart:self.xlen];
}

<*
 Get the value at a particular x/y position in the slice.

 @param coord : "The xy coordinate"
 @return "The value at that coordinate"
 @require coord.y >= 0 && coord.y < self.ylen : "y value out of range"
 @require coord.x >= 0 && coord.x < self.xlen : "x value out of range"
*>
macro Type Slice2d.get_coord(self, usz[<2>] coord)
{
	return *self.get_coord_ref(coord);
}

<*
 Get a pointer to the value at a particular x/y position in the slice.

 @param coord : "The xy coordinate"
 @return "A pointer to the value at that coordinate"
 @require coord.y >= 0 && coord.y < self.ylen : "y value out of range"
 @require coord.x >= 0 && coord.x < self.xlen : "x value out of range"
*>
macro Type* Slice2d.get_coord_ref(self, usz[<2>] coord)
{
	return self.get_xy_ref(coord.x, coord.y);
}

<*
 Get the value at a particular x/y position in the slice.

 @param x : "The x coordinate"
 @param y : "The x coordinate"
 @return "The value at that coordinate"
 @require y >= 0 && y < self.ylen : "y value out of range"
 @require x >= 0 && x < self.xlen : "x value out of range"
*>
macro Type Slice2d.get_xy(self, x, y)
{
	return *self.get_xy_ref(x, y);
}

<*
 Get the value at a particular x/y position in the slice by reference.

 @param x : "The x coordinate"
 @param y : "The y coordinate"
 @return "A pointer to the value at that coordinate"
 @require y >= 0 && y < self.ylen : "y value out of range"
 @require x >= 0 && x < self.xlen : "x value out of range"
*>
macro Type* Slice2d.get_xy_ref(self, x, y)
{
	return self.ptr + self.inner_len * (y + self.ystart) + self.xstart + x;
}

<*
 Set the ´value at a particular x/y position in the slice.

 @param coord : "The xy coordinate"
 @param value : "The new value"
 @require coord.y >= 0 && coord.y < self.ylen : "y value out of range"
 @require coord.x >= 0 && coord.x < self.xlen : "x value out of range"
*>
macro void Slice2d.set_coord(self, usz[<2>] coord, Type value)
{
	*self.get_coord_ref(coord) = value;
}

<*
 Set the value at a particular x/y position in the slice.

 @param x : "The x coordinate"
 @param y : "The y coordinate"
 @param value : "The new value"
 @require y >= 0 && y < self.ylen : "y value out of range"
 @require x >= 0 && x < self.xlen : "x value out of range"
*>
macro void Slice2d.set_xy(self, x, y, Type value)
{
	*self.get_xy_ref(x, y) = value;
}

<*
 Reslice a slice2d returning a new slice.

 @param x : "The starting x"
 @param xlen : "The length along x"
 @param y : "The starting y"
 @param ylen : "The length along y"
 @require y >= 0 && y < self.ylen
 @require x >= 0 && x < self.xlen
*>
fn Slice2d Slice2d.slice(&self, isz x = 0, isz xlen = 0, isz y = 0, isz ylen = 0)
{
	if (xlen < 1) xlen = self.xlen + xlen;
	if (ylen < 1) ylen = self.ylen + ylen;
	return { self.ptr, self.inner_len, y + self.ystart, ylen, x + self.xstart, xlen };
}

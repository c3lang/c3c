module std::core::array;

<*
 A slice2d allows slicing an array like int[10][10] into an arbitrary "int[][]"-like counterpart
 Typically you'd use array::slice2d(...) to create one.
*>
struct Slice2d <Type>
{
	Type* ptr;
	sz inner_len;
	sz ystart;
	sz ylen;
	sz xstart;
	sz xlen;
}

<*
 @return `The length of the "outer" slice`
*>
fn sz Slice2d.len(&self) @operator(len)
{
	return self.ylen;
}

<*
 @return `The total number of elements.`
*>
fn sz Slice2d.count(&self)
{
	return self.ylen * self.xlen;
}

<*
 Step through each element of the slice.
*>
macro void Slice2d.@each(&self; @body(sz[<2>], Type))
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
macro void Slice2d.@each_ref(&self; @body(sz[<2>], Type*))
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

 @param y : "The row to return"
 @return "The slice for the particular row"
 @require [y] (usz)y < (usz)self.ylen : "y value out of range"
*>
macro Type[] Slice2d.get_row(self, sz y) @operator([])
{
	return (self.ptr + self.inner_len * (y + self.ystart))[self.xstart:self.xlen];
}

<*
 Get the value at a particular x/y position in the slice.

 @param coord : "The xy coordinate"
 @return "The value at that coordinate"

 @require (usz)coord.x < (usz)self.xlen : "x value out of range"
 @require (usz)coord.y < (usz)self.ylen : "y value out of range"
*>
macro Type Slice2d.get_coord(self, sz[<2>] coord)
{
	return *self.get_coord_ref(coord);
}

<*
 Get a pointer to the value at a particular x/y position in the slice.

 @param coord : "The xy coordinate"
 @return "A pointer to the value at that coordinate"

 @require (usz)coord.x < (usz)self.xlen : "x value out of range"
 @require (usz)coord.y < (usz)self.ylen : "y value out of range"
*>
macro Type* Slice2d.get_coord_ref(self, sz[<2>] coord)
{
	return self.get_xy_ref(coord.x, coord.y);
}

<*
 Get the value at a particular x/y position in the slice.

 @param x : "The x coordinate"
 @param y : "The x coordinate"
 @return "The value at that coordinate"

 @require [x] (usz)x < (usz)self.xlen : "x value out of range"
 @require [y] (usz)y < (usz)self.ylen : "y value out of range"
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

 @require [x] (usz)x < (usz)self.xlen : "x value out of range"
 @require [y] (usz)y < (usz)self.ylen : "y value out of range"
*>
macro Type* Slice2d.get_xy_ref(self, x, y)
{
	return self.ptr + self.inner_len * (y + self.ystart) + self.xstart + x;
}

<*
 Set the ´value at a particular x/y position in the slice.

 @param coord : "The xy coordinate"
 @param value : "The new value"

 @require (usz)coord.x < (usz)self.xlen : "x value out of range"
 @require (usz)coord.y < (usz)self.ylen : "y value out of range"
*>
macro void Slice2d.set_coord(self, sz[<2>] coord, Type value)
{
	*self.get_coord_ref(coord) = value;
}

<*
 Set the value at a particular x/y position in the slice.

 @param x : "The x coordinate"
 @param y : "The y coordinate"
 @param value : "The new value"

 @require [x] (usz)x < (usz)self.xlen : "x value out of range"
 @require [y] (usz)y < (usz)self.ylen : "y value out of range"
*>
macro void Slice2d.set_xy(self, x, y, Type value)
{
	*self.get_xy_ref(x, y) = value;
}

<*
 Reslice a slice2d returning a new slice.

 For xlen and ylen, a negative value instead trims that many elements off the end of the row,
 e.g. -1 stops one element short of the end. 0 gives the full length.

 @param x : "The starting x"
 @param xlen : "The length along x."
 @param y : "The starting y"
 @param ylen : "The length along y"
 @return "A new slice"

 @require (usz)x < (usz)self.xlen : "x position out of bounds"
 @require (usz)y < (usz)self.ylen : "y position out of bounds"
 @require xlen < 1 || x + xlen <= self.xlen : "xlen out of bounds"
 @require ylen < 1 || y + ylen <= self.ylen : "ylen out of bounds"
 @require xlen > 0 || self.xlen + xlen >= x : "xlen out of bounds"
 @require ylen > 0 || self.ylen + ylen >= y : "ylen out of bounds"
*>
fn Slice2d Slice2d.slice(&self, sz x = 0, sz xlen = 0, sz y = 0, sz ylen = 0)
{
	if (xlen < 1) xlen = self.xlen - x + xlen;
	if (ylen < 1) ylen = self.ylen - y + ylen;
	return { self.ptr, self.inner_len, y + self.ystart, ylen, x + self.xstart, xlen};
}

<*
 @require TreeValue::is_ordered : `TreeValue must be ordered`
*>
module std::collections::set <TreeValue>;
import std::io @norecurse;

const Allocator TREE_HEAP_ALLOCATOR = (Allocator)&dummy;

<* Copy the ONHEAP allocator to initialize to a set that is heap allocated *>
const TreeSet TREEONHEAP = { .allocator = TREE_HEAP_ALLOCATOR };

enum NodeColor @private
{
	RED,
	BLACK
}

struct Node
{
	NodeColor color;
	Node* left;
	Node* right;
	Node* parent;
	TreeValue value;
}

struct TreeSet (Printable)
{
	Allocator allocator;
	Node* root;
	sz count;
}

<*
 @param [&inout] allocator : "The allocator to use"
 @require !self.is_initialized() : "TreeSet was already initialized"
*>
fn TreeSet* TreeSet.init(&self, Allocator allocator)
{
	*self = { .allocator = allocator };
	return self;
}

<*
 @require !self.is_initialized() : "TreeSet was already initialized"
*>
fn TreeSet* TreeSet.tinit(&self)
{
	return self.init(tmem) @inline;
}

<*
 @param [in] values : "The values to initialize the tree with"
 @param [&inout] allocator : "The allocator to use"
 @require !self.is_initialized() : "TreeSet was already initialized"
*>
fn TreeSet* TreeSet.init_from_values(&self, Allocator allocator, TreeValue[] values)
{
	self.init(allocator);
	self.add_all(values);
	return self;
}

<*
 @param [in] values : "The values to initialize the tree with"
 @require !self.is_initialized() : "TreeSet was already initialized"
*>
fn TreeSet* TreeSet.tinit_from_values(&self, TreeValue[] values) @inline => self.init_from_values(tmem, values);

<*
 @param [&inout] allocator : "The allocator to use"
 @require !self.is_initialized() : "TreeSet was already initialized"
*>
macro TreeSet* TreeSet.init_with_values(&self, Allocator allocator, ...)
{
	self.init(allocator);
	$for var $i = 0; $i < $vaarg.len; $i++:
		self.add($vaarg[$i]);
	$endfor
	return self;
}

<*
 @require !self.is_initialized() : "TreeSet was already initialized"
*>
macro TreeSet* TreeSet.tinit_with_values(&self, ...)
{
	return self.init_with_values(tmem, ...$vaarg);
}

<*
 @param [&inout] allocator : "The allocator to use"
 @param [&in] other_tree : "The tree to copy from."
 @require !self.is_initialized() : "TreeSet was already initialized"
*>
fn TreeSet* TreeSet.init_from_tree(&self, Allocator allocator, TreeSet* other_tree)
{
	self.init(allocator);
	other_tree.@each(; TreeValue value)
	{
		self.add(value);
	};
	return self;
}

<*
 @param [&in] other_tree : "The tree to copy from."
 @require !self.is_initialized() : "TreeSet was already initialized"
*>
fn TreeSet* TreeSet.tinit_from_tree(&self, TreeSet* other_tree) @inline => self.init_from_tree(tmem, other_tree);

<*
 Has the tree been initialized yet?
 @param [&in] self : "The tree to check"
 @return "true if the tree has been initialized, false otherwise"
*>
fn bool TreeSet.is_initialized(&self) @inline => self.allocator != null;

<*
 Add all values from a slice to the tree.
 @param [in] values
 @return "the number of values added"
 @ensure return <= values.len
*>
fn sz TreeSet.add_all(&self, TreeValue[] values)
{
	sz added_count = 0;
	foreach (value : values)
	{
		if (self.add(value)) added_count++;
	}
	return added_count;
}

<*
 Add all values from another tree to this tree.
 @param [&in] other
 @return "the number of values added"
 @ensure return <= other.count
*>
fn sz TreeSet.add_all_from(&self, TreeSet* other)
{
	sz added_count = 0;
	other.@each(; TreeValue value)
	{
		if (self.add(value)) added_count++;
	};
	return added_count;
}

<*
 Add a value to the tree.
 @param value
 @return "true if the value was added, false if it was already present"
*>
fn bool TreeSet.add(&self, TreeValue value)
{
	bool added = tree_add_and_rebalance(self, value);
	if (added) self.count++;
	return added;
}

<*
 Free all nodes in the tree.
 @ensure self.is_empty()
*>
fn void TreeSet.free(&self) => self.clear() @inline;

fn void TreeSet.clear(&self)
{
	Node* node = self.root;
	while (node)
	{
		Node* next = node.left ? node.left : node.right;
		if (!next)
		{
			next = node.parent;
			tree_free_node(self, node);
			if (next)
			{
				if (node == next.left)
				{
					next.left = null;
				}
				else
				{
					next.right = null;
				}
			}
		}
		node = next;
	}
	self.root = null;
	self.count = 0;
}

<*
 Remove all values from a slice from the tree.
 @param [in] values
 @return "the number of values removed"
 @ensure return <= values.len
*>
fn sz TreeSet.remove_all(&self, TreeValue[] values)
{
	sz removed_count = 0;
	foreach (value : values)
	{
		if (@ok(self.remove(value))) removed_count++;
	}
	return removed_count;
}

<*
 Remove all values from another tree from this tree.
 @param [&in] other
 @return "the number of values removed"
 @ensure return <= other.count
*>
fn sz TreeSet.remove_all_from(&self, TreeSet* other)
{
	sz removed_count = 0;
	other.@each(; TreeValue value)
	{
		if (@ok(self.remove(value))) removed_count++;
	};
	return removed_count;
}

<*
 Remove a value from the tree.
 @param value
 @return? tree_remove_and_rebalance!
*>
fn void? TreeSet.remove(&self, TreeValue value) @maydiscard
{
	Node* z = tree_remove_and_rebalance(self, value)!;
	tree_free_node(self, z);
	self.count--;
}

fn bool TreeSet.contains(&self, TreeValue value) @inline => @ok(tree_find(self, value));

<*
 Iterate over all values in the tree
*>
macro void TreeSet.@each(tree; @body(value))
{
	if (tree.is_empty()) return;
	Node* node = tree_minimum(tree.root);

	while (true)
	{
		@body(node.value);
		if (node.right)
		{
			node = tree_minimum(node.right);
			continue;
		}
		else
		{
			Node* parent = node.parent;
			if (!parent) return;
			while (node == parent.right)
			{
				node = parent;
				parent = node.parent;
				if (!parent) return;
			}
			node = parent;
			continue;
		}
	}
}

fn bool TreeSet.is_empty(&self) @inline => !self.count;

fn sz TreeSet.len(&self) @inline => self.count;

fn TreeValue[] TreeSet.tvalues(&self) @inline => self.values(tmem);

fn TreeValue[] TreeSet.values(&self, Allocator allocator)
{
	TreeValue[] list = alloc::alloc_array(allocator, TreeValue, self.count);
	sz index;
	self.@each(; TreeValue value)
	{
		list[index++] = value;
	};
	return list;
}


// --- Private functions ---

<*
 @require self.is_initialized()
*>
macro void tree_free_node(TreeSet* self, Node* node) @private
{
	alloc::free(self.allocator, node);
}

macro Node* tree_alloc_node(TreeSet* self, TreeValue value) @private
{
	// Initialize tree with defaults if not yet initialized.
	switch (self.allocator.ptr)
	{
		case &dummy:
			self.init(mem);
		case null:
			self.tinit();
		default:
			break;
	}
	return alloc::new(self.allocator, Node, { .value = value, .color = RED });
}

<*
 @return? NOT_FOUND
*>
fn Node*? tree_find(TreeSet* self, TreeValue value) @private
{
	Node* x = self.root;
	while (x != null)
	{
		if (value == x.value) return x;
		x = value < x.value ? x.left : x.right;
	}
	return NOT_FOUND~;
}

fn void tree_rotate_left(TreeSet* self, Node* x) @private
{
	Node* y = x.right;
	x.right = y.left;
	if (y.left) y.left.parent = x;
	y.parent = x.parent;
	switch (x)
	{
		case self.root:     self.root = y;
		case x.parent.left: x.parent.left = y;
		default:            x.parent.right = y;
	}
	y.left = x;
	x.parent = y;
}

fn void tree_rotate_right(TreeSet* self, Node* y) @private
{
	Node* x = y.left;
	y.left = x.right;
	if (x.right) x.right.parent = y;
	x.parent = y.parent;
	switch (y)
	{
		case self.root:      self.root = x;
		case y.parent.right: y.parent.right = x;
		default:             y.parent.left = x;
	}
	x.right = y;
	y.parent = x;
}

fn bool tree_add_and_rebalance(TreeSet* self, TreeValue value) @private
{
	Node* x = self.root;
	Node* y;
	Node* z;

	while (x)
	{
		if (value == x.value) return false;
		y = x;
		if (value < x.value)
		{
			x = x.left;
		}
		else
		{
			x = x.right;
		}
	}
	z = tree_alloc_node(self, value);
	z.parent = y;
	if (!y)
	{
		self.root = z;
	}
	else if (value < y.value)
	{
		y.left = z;
	}
	else
	{
		y.right = z;
	}

	// Fixup of RB structure after insertion

	while (z != self.root && z.parent.color == RED)
	{
		if (z.parent == z.parent.parent.left)
		{
			y = z.parent.parent.right;
			if (y && y.color == RED)
			{
				z.parent.color = BLACK;
				y.color = BLACK;
				z.parent.parent.color = RED;
				z = z.parent.parent;
			}
			else
			{
				if (z == z.parent.right)
				{
					z = z.parent;
					tree_rotate_left(self, z);
				}
				z.parent.color = BLACK;
				z.parent.parent.color = RED;
				tree_rotate_right(self, z.parent.parent);
			}
		}
		else
		{
			y = z.parent.parent.left;
			if (y && y.color == RED)
			{
				z.parent.color = BLACK;
				y.color = BLACK;
				z.parent.parent.color = RED;
				z = z.parent.parent;
			}
			else
			{
				if (z == z.parent.left)
				{
					z = z.parent;
					tree_rotate_right(self, z);
				}
				z.parent.color = BLACK;
				z.parent.parent.color = RED;
				tree_rotate_left(self, z.parent.parent);
			}
		}
	}
	self.root.color = BLACK;
	return true;
}

fn void tree_transplant(TreeSet* self, Node* u, Node* v) @private
{
	switch
	{
		case !u.parent:          self.root = v;
		case u == u.parent.left: u.parent.left = v;
		default:                 u.parent.right = v;
	}
	if (v) v.parent = u.parent;
}

fn Node* tree_minimum(Node* node) @private
{
	while (node.left) node = node.left;
	return node;
}

fn Node* tree_maximum(Node* node) @private
{
	while (node.right) node = node.right;
	return node;
}

<*
 @return? tree_find!
*>
fn Node*? tree_remove_and_rebalance(TreeSet* self, TreeValue value) @private
{
	Node* z           = tree_find(self, value)!;
	Node* y           = z;
	Node* x; // Child that will replace z
	Node* x_parent;
	NodeColor y_color = y.color;

	switch
	{
		case !z.left:
			x = z.right;
			x_parent = z.parent;
			tree_transplant(self, z, z.right);
		case !z.right:
			x = z.left;
			x_parent = z.parent;
			tree_transplant(self, z, z.left);
		default:
			y = tree_minimum(z.right);
			y_color = y.color;
			x = y.right;
			x_parent = y;
			if (y != z.right)
			{
				tree_transplant(self, y, y.right);
				x_parent = y.parent;
				y.right = z.right;
				if (y.right != null) y.right.parent = y;
			}
			tree_transplant(self, z, y);
			y.left = z.left;
			if (y.left != null) y.left.parent = y;
			y.color = z.color;
	}

	// Rebalance after deletion
	if (y_color == BLACK)
	{
		while (x != self.root && (!x || x.color == BLACK))
		{
			if (x == x_parent.left)
			{
				Node* w = x_parent.right;
				if (w.color == RED)
				{
					w.color = BLACK;
					x_parent.color = RED;
					tree_rotate_left(self, x_parent);
					w = x_parent.right;
				}
				if ((!w.left || w.left.color == BLACK)
					&& (!w.right || w.right.color == BLACK))
				{
					w.color = RED;
					x = x_parent;
					x_parent = x.parent;
				}
				else
				{
					if (!w.right || w.right.color == BLACK)
					{
						w.left.color = BLACK;
						w.color = RED;
						tree_rotate_right(self, w);
						w = x_parent.right;
					}
					w.color = x_parent.color;
					x_parent.color = BLACK;
					if (w.right) w.right.color = BLACK;
					tree_rotate_left(self, x_parent);
					x = self.root;
				}
			}
			else
			{
				Node* w = x_parent.left;
				if (w.color == RED)
				{
					w.color = BLACK;
					x_parent.color = RED;
					tree_rotate_right(self, x_parent);
					w = x_parent.left;
				}
				if ((!w.right || w.right.color == BLACK)
					&& (!w.left || w.left.color == BLACK))
				{
					w.color = RED;
					x = x_parent;
					x_parent = x.parent;
				}
				else
				{
					if (!w.left || w.left.color == BLACK)
					{
						w.right.color = BLACK;
						w.color = RED;
						tree_rotate_left(self, w);
						w = x_parent.left;
					}
					w.color = x_parent.color;
					x_parent.color = BLACK;
					if (w.left) w.left.color = BLACK;
					tree_rotate_right(self, x_parent);
					x = self.root;
				}
			}
		}
		if (x) x.color = BLACK;
	}
	return z;
}

fn sz? TreeSet.to_format(&self, Formatter* f) @dynamic
{
	sz len = f.print("{ ")!;
	self.@each(; TreeValue value)
	{
		if (len > 2) len += f.print(", ")!;
		len += f.printf("%s", value)!;
	};
	return len + f.print(" }");
}

struct TreeSetIterator
{
	TreeSet* tree;
	Node* current;
	bool started;
}

fn TreeSetIterator TreeSet.iter(&self) => { .tree = self, .started = false };

<*
 @return? NOT_FOUND
*>
fn TreeValue*? TreeSetIterator.get(&self)
{
	return self.current ? &self.current.value : NOT_FOUND~;
}

fn bool TreeSetIterator.next(&self)
{
	if (!self.started)
	{
		self.started = true;
		self.current = self.tree.root ? tree_minimum(self.tree.root) : null;
	}
	else if (self.current.right)
	{
		self.current = tree_minimum(self.current.right);
	}
	else
	{
		Node* parent = self.current.parent;
		while (parent && self.current == parent.right)
		{
			self.current = parent;
			parent = parent.parent;
		}
		self.current = parent;
	}
	return self.current != null;
}

fn sz TreeSetIterator.len(&self) @operator(len) => self.tree.count;

int dummy @local;

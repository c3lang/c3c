module test;

// An interface named through an alias is the same interface: a struct implementing it converts
// to it, and an interface inheriting through an alias converts to its parents.
interface Iface
{
	fn void hi();
}

alias IAlias = Iface;

struct ViaAlias (IAlias)
{
	int x;
}

fn void ViaAlias.hi(&self) @dynamic {}

interface Parent
{
	fn void p();
}

interface Mid : Parent
{
	fn void m();
}

alias MidAlias = Mid;

interface Leaf : MidAlias
{
	fn void l();
}

fn void struct_to_interface() { ViaAlias v; Iface i = &v; }
fn void struct_to_alias() { ViaAlias v; IAlias i = &v; }
fn void leaf_to_mid(Leaf l) { Mid m = l; }
fn void leaf_to_parent(Leaf l) { Parent p = l; }

// The interfaces that are not implemented, or are not parents, must still be rejected.
interface Other
{
	fn void o();
}

fn void not_implemented() { ViaAlias v; Other o = &v; } // #error: cannot be implicitly cast
fn void parent_to_leaf(Parent p) { Leaf l = p; } // #error: not a parent interface

// TODO: ensure the type is an enum first.
module enumset<Enum>;

$assert(Enum.min < Enum.max, "Only strictly increasing enums may be used with enum sets.");
$assert(Enum.max < 64, "Maximum value of an enum used as enum set is 63");
$assert(Enum.min >= 0, "Minimum value of an enum used as enum set is 0");

$if $$C_INT_SIZE == 64:
private define EnumSetType = ulong;

$elif $$C_INT_SIZE == 32:

$if Enum.max < 32:
private define EnumSetType = uint;
$else:
private define EnumSetType = ulong;
$endif;

$else:

$if Enum.max < 16:
private define EnumSetType = ushort;
$elif Enum.max < 31:
private define EnumSetType = uint;
$else:
private define EnumSetType = ulong;
$endif;

$endif;

define EnumSet = distinct EnumSetType;

fn void EnumSet.add(EnumSet *this, Enum v)
{
    *this = (EnumSet)((EnumSetType)*this | 1u << (EnumSetType)v);
}

fn void EnumSet.clear(EnumSet *this)
{
    *this = 0;
}

fn bool EnumSet.remove(EnumSet *this, Enum v)
{
    EnumSetType old = (EnumSetType)*this;
    EnumSetType new = old & ~(1u << (EnumSetType)v);
    *this = (EnumSet)new;
    return old != new;
}

fn bool EnumSet.has(EnumSet *this, Enum v)
{
    return ((EnumSetType)*this & (1u << (EnumSetType)v)) != 0;
}

fn void EnumSet.add_all(EnumSet *this, EnumSet s)
{
    *this = (EnumSet)((EnumSetType)*this | (EnumSetType)s);
}

fn void EnumSet.retain_all(EnumSet *this, EnumSet s)
{
    *this = (EnumSet)((EnumSetType)*this & (EnumSetType)s);
}

fn EnumSet EnumSet.and_of(EnumSet *this, EnumSet s)
{
    return (EnumSet)((EnumSetType)*this & (EnumSetType)s);
}

fn EnumSet EnumSet.or_of(EnumSet *this, EnumSet s)
{
    return (EnumSet)((EnumSetType)*this | (EnumSetType)s);
}

fn EnumSet EnumSet.diff_of(EnumSet *this, EnumSet s)
{
    return (EnumSet)((EnumSetType)*this & ~(EnumSetType)s);
}

fn EnumSet EnumSet.xor_of(EnumSet *this, EnumSet s)
{
    return (EnumSet)((EnumSetType)*this ^ (EnumSetType)s);
}
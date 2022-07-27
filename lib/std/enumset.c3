// TODO: ensure the type is an enum first.
module std::enumset<Enum>;

$assert(Enum.elements < 64, "Maximum number of elements for an enum used as enum set is 63");

$switch ($$C_INT_SIZE):
$case 64:
    private define EnumSetType = ulong;
$case 32:
    $if (Enum.elements < 32):
        private define EnumSetType = uint;
    $else:
        private define EnumSetType = ulong;
    $endif;
$default:
    $if (Enum.elements < 16):
        private define EnumSetType = ushort;
    $elif (Enum.elements < 31):
        private define EnumSetType = uint;
    $else:
        private define EnumSetType = ulong;
    $endif;
$endswitch;

define EnumSet = distinct EnumSetType;

fn void EnumSet.add(EnumSet* this, Enum v)
{
    *this = (EnumSet)((EnumSetType)*this | 1u << (EnumSetType)v);
}

fn void EnumSet.clear(EnumSet* this)
{
    *this = 0;
}

fn bool EnumSet.remove(EnumSet* this, Enum v)
{
    EnumSetType old = (EnumSetType)*this;
    EnumSetType new = old & ~(1u << (EnumSetType)v);
    *this = (EnumSet)new;
    return old != new;
}

fn bool EnumSet.has(EnumSet* this, Enum v)
{
    return ((EnumSetType)*this & (1u << (EnumSetType)v)) != 0;
}

fn void EnumSet.add_all(EnumSet* this, EnumSet s)
{
    *this = (EnumSet)((EnumSetType)*this | (EnumSetType)s);
}

fn void EnumSet.retain_all(EnumSet* this, EnumSet s)
{
    *this = (EnumSet)((EnumSetType)*this & (EnumSetType)s);
}

fn void EnumSet.remove_all(EnumSet* this, EnumSet s)
{
    *this = (EnumSet)((EnumSetType)*this & ~(EnumSetType)s);
}

fn EnumSet EnumSet.and_of(EnumSet* this, EnumSet s)
{
    return (EnumSet)((EnumSetType)*this & (EnumSetType)s);
}

fn EnumSet EnumSet.or_of(EnumSet* this, EnumSet s)
{
    return (EnumSet)((EnumSetType)*this | (EnumSetType)s);
}

fn EnumSet EnumSet.diff_of(EnumSet* this, EnumSet s)
{
    return (EnumSet)((EnumSetType)*this & ~(EnumSetType)s);
}

fn EnumSet EnumSet.xor_of(EnumSet* this, EnumSet s)
{
    return (EnumSet)((EnumSetType)*this ^ (EnumSetType)s);
}
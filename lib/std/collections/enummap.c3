module std::collections::enummap<Enum, ValueType>;

struct EnumMap
{
    ValueType[Enum.len] values;
}

fn void EnumMap.init(EnumMap* this, ValueType init_value)
{
    foreach(&a : this.values)
    {
        *a = init_value;
    }
}

fn uint EnumMap.len(EnumMap* this) @operator(len) => this.values.len;
fn ValueType EnumMap.get(EnumMap* this, Enum key) @operator([]) => this.values[key.ordinal];
fn void EnumMap.set(EnumMap* this, Enum key, ValueType value) @operator([]=) => this.values[key.ordinal] = value;

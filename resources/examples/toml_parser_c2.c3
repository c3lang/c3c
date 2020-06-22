// This is the toml_parser.c2 changed to c3 to compare

module toml;

import stdio;
import stdlib;
import string;
import file_utils;
import csetjmp;

const uint NamesCacheSize = 8;
const uint MaxNodes = 1024;
const uint MaxNames = 4096;
const uint MaxValues = 4096 * 128;
const uint MaxDepth = 8;

//#define DEBUG_NODES

$if (DEBUG_NODES):

func void Blocks.dump(Blocks* b)
{
    printf("Nodes (%u/%u)  (%u bytes)\n", b.nodeCount, b.maxNodes, b.nodeCount * sizeof(Node));
    for (uint i = 0; i < b.nodeCount; i++)
    {
        // @ensure const(n)
        Node* n = &b.nodes[i];
        uint nameOffset = getValue(n.nameOffset);
        NodeKind kind = getKind(n.nameOffset);
        switch (kind)
        {
            case NodeKind.TABLE:
            case NodeKind.TABLE_ARRAY:
                printf("  [%3u]  %s name %3u   next %3u   child %3u  (%s)\n",
                    i, kind.name, nameOffset, n.nextNode, n.child, &b.names[nameOffset]);
            case NodeKind.VALUE_ARRAY:
            case NodeKind.VALUE:
                ValueType t = getRawType(n.rawValue);
                uint offset = getRawValue(n.rawValue);
                printf("  [%3u]  %s name %3u   next %3u   value %5u(%s)  (%s)\n",
                    i, kind.name, nameOffset, n.nextNode, offset, type2str(t), &b.names[nameOffset]);
            case NodeKind.ARRAY_CHILD:
                printf("  [%3u]  %s name ---   next %3u   child %3u\n",
                    i, kind.name, n.nextNode, n.child);
        }
    }
    printf("Names (%u/%u)\n", b.namesOffset, b.namesSize);
    uint i = 1;
    uint start = i;
    while (i < b.namesOffset)
    {
        if (b.names[i] == 0) {
            printf("  [%3u] %s\n", start, &b.names[start]);
            i++;
            start = i;
        }
        else
        {
            i++;
        }
    }
    printf("Values (%u/%u)\n", b.valuesOffset, b.valuesSize);
    i = 1;
    start = i;
    while (i < b.valuesOffset)
    {
        if (b.values[i] == 0)
        {
            printf("  [%3u] %s\n", start, &b.values[start]);
            i++;
            start = i;
        }
        else
        {
            i++;
        }
    }
}

$endif;

/**
 * @ensure const(a), const(b)
 */
func bool same(char* a, char* b)
{
    uint i = 0;
    while (a[i] == b[i])
    {
        if (a[i] == 0) return true;
        ++i;
    }
    return false;
}

struct Parser
{
    Tokenizer tokenizer;
    Token tok;
    JmpBuf jump_err;
    char* errorMsg;

    Blocks* blocks;
    Node*[MaxDepth] parents;
    Node*[MaxDepth] lastChild;
    uint numParents;
    Node* topParent;
}

/**
 * @ensure const(input)
 */
func void! Parser.parse(Parser* p, char* input, char* diagMsg, Blocks* blocks)
{
    p.tokenizer.init(input);
    p.tok.init();
    p.errorMsg = diagMsg;
    p.errorMsg[0] = 0;

    p.blocks = blocks;
    memset(p.parents, 0, sizeof(Node*)*MaxDepth);
    memset(p.lastChild, 0, sizeof(Node*)*MaxDepth);
    p.numParents = 0;
    p.topParent = nil;

    try p.consumeToken();
    try p.parseTopLevel();
}

func void! Parser.parseTopLevel(Parser* p)
{
    // key = value
    // | [[array]]
    // | [table]
    while (p.tok.isNot(TokenKind.EOF))
    {
        switch (p.tok.kind)
        {
            case WORD:
                p.parseKeyValue();
            case LBRACE:
                p.parseTable();
            case LBRACE2:
                p.parseTableArray();
            default:
                sprintf(p.errorMsg, "syntax error %s", p.tok.loc.str());
                return SyntaxError!;
        }
    }
}

func uint getRawValue(uint raw) @(inline)
{
     return raw & ~RawValueMask;
}

func ValueType getRawType(uint raw) @(inline)
{
    return cast((raw >> ValueTypeOffset) & 0x3, ValueType);
}

func uint addType(uint raw, ValueType t) @(inline)
{
    return raw | (t << ValueTypeOffset);
}

func void! Parser.parseKeyValue(Parser* p)
{
    //printf("parseKeyValue()\n");
    char[MaxText] key;
    strcpy(key, p.tok.text);
    try p.consumeToken();
    try p.expectAndConsume(TokenKind.Equals);
    u32 value = try p.parseValue();
    bool isArray = value & ValueIsArray != 0;
    u32 off = p.blocks.addNode(key, isArray ? NodeKind.ValueArray : NodeKind.Value);
    Node* node = &p.blocks.nodes[off];
    node.rawValue = value;
    if (p.lastChild[p.numParents])
    {
        p.lastChild[p.numParents].nextNode = off;
    }
    else
    {
        if (p.topParent) p.topParent.child = off;
    }
    p.lastChild[p.numParents] = node;
}

func void! Parser.parseTable(Parser* p)
{
    //printf("parseTable()\n");
    try p.consumeToken();
    try p.expect(TokenKind.Word);
    char* name = p.tok.text;
    uint depth = 0;
    bool isTop = p.nextToken().isNot(TokenKind.DOT);
    depth += p.addTable(name, depth, isTop, NodeKind.Table);
    p.consumeToken();

    while (p.tok.is(TokenKind.DOT))
    {
        depth++;
        p.consumeToken();
        p.expect(TokenKind.WORD);
        name = p.tok.text;
        isTop = p.nextToken().isNot(TokenKind.DOT);
        depth += p.addTable(name, depth, isTop, NodeKind.TABLE);
        p.consumeToken();
    }
    p.expectAndConsume(TokenKind.Rbrace);
}

func void Parser.parseTableArray(Parser* p)
{
    //printf("parseTableArray()\n");
    p.consumeToken();
    p.expect(TokenKind.Word);
    const char* name = p.tok.text;
    u32 depth = 0;
    bool isTop = p.nextToken().isNot(TokenKind.Dot);
    depth += p.addTable(name, depth, isTop, NodeKind.TableArray);
    p.consumeToken();

    while (p.tok.is(TokenKind.Dot)) {
        depth++;
        p.consumeToken();
        p.expect(TokenKind.Word);
        name = p.tok.text;
        isTop = p.nextToken().isNot(TokenKind.Dot);
        depth += p.addTable(name, depth, isTop, NodeKind.TableArray);
        p.consumeToken();
    }
    p.expectAndConsume(TokenKind.Rbrace2);
}

func u32 Parser.parseValue(Parser* p) {
    //printf("parseValue()\n");
    u32 value = 0;
    switch (p.tok.kind) {
    case TokenKind.Word:
        sprintf(p.errorMsg, "unexpected word at %s", p.tok.loc.str());
        longjmp(p.jump_err, 1);
        break;
    case TokenKind.Text:
        value = p.blocks.addValue(p.tok.text);
        value = addType(value, ValueType.Text);
        p.consumeToken();
        break;
    case TokenKind.Number:
        // TODO negative numbers
        value = addType(p.tok.number, ValueType.Number);
        p.consumeToken();
        break;
    case TokenKind.Kw_true: fallthrough;
    case TokenKind.Kw_false:
        value = addType(p.tok.number, ValueType.Boolean);
        p.consumeToken();
        break;
    case TokenKind.Lbrace:
        value = p.parseArrayValues();
        break;
    default:
        break;
    }
    return value;
}

func u32 Parser.parseArrayValues(Parser* p) {
    //printf("parseArrayValues()\n");
    p.consumeToken();
    u32 value = p.parseValue() | ValueIsArray;
    while (p.tok.is(TokenKind.Comma)) {
        p.consumeToken();
        if (p.tok.is(TokenKind.Rbrace)) break;  // trailing comma is allowed
        p.parseValue();
    }
    p.expectAndConsume(TokenKind.Rbrace);
    p.blocks.addNull();
    return value;
}

func u32 Parser.addTable(Parser* p, const char* name, u32 depth, bool isTop, NodeKind kind) {
    //printf("addTable %s\n", name);
    Blocks* blocks = p.blocks;
    if (!isTop && p.numParents > depth && same(blocks.getName(p.parents[depth]), name)) {
        if (getKind(p.parents[depth].nameOffset) == NodeKind.TableArray) return 1;
        // Do nothing
    } else {
        if (kind == NodeKind.TableArray) {
            // TODO also check if previous is also TableArray
            if (p.numParents > depth && same(blocks.getName(p.parents[depth]), name)) {
                p.numParents = depth + 1;
            } else {
                u32 off = blocks.addNode(name, kind);
                if (p.numParents > depth) p.parents[depth].nextNode = off;
                Node* node = &blocks.nodes[off];
                p.parents[depth] = node;

                if (p.lastChild[depth]) {
                    p.lastChild[depth].nextNode = off;
                } else {
                    if (depth > 0) p.parents[depth - 1].child = off;
                }
                p.numParents = depth + 1;
                p.topParent = node;
                p.lastChild[depth] = node;
                p.lastChild[depth + 1] = nil;
            }
            if (isTop) {
                // add iterator node as child or next
                u32 off = blocks.addNode("", NodeKind.ArrayChild);
                Node* iter = &blocks.nodes[off];
                if (p.lastChild[depth].child) { // already has children
                    p.lastChild[depth + 1].nextNode = off;
                } else {
                    p.lastChild[depth].child = off;
                }
                p.lastChild[depth + 1] = iter;
                p.parents[depth + 1] = iter;
                p.lastChild[depth + 2] = nil;
                p.topParent = iter;
                p.numParents++;
            }
            return 1;
        }
        u32 off = blocks.addNode(name, kind);
        if (p.numParents > depth) p.parents[depth].nextNode = off;
        Node* node = &blocks.nodes[off];
        p.parents[depth] = node;

        if  (p.lastChild[depth]) {
            p.lastChild[depth].nextNode = off;
        } else {
            if (depth > 0) p.parents[depth-1].child = off;
        }
        p.numParents = depth + 1;
        p.topParent = node;
        p.lastChild[depth] = node;
        p.lastChild[depth + 1] = nil;
    }
    return 0;
}

func Location! Parser.consumeToken(Parser* p)
{
    Location prev = p.tok.loc;
    try p.tokenizer.lex(&p.tok);
    return prev;
}

func Token* Parser.nextToken(Parser* p) {
    return p.tokenizer.lookahead();
}

func void! Parser.expectAndConsume(Parser* p, TokenKind k) {
    if (p.tok.isNot(k))
    {
        sprintf(p.errorMsg, "expected '%s' at %s", token2str(k), p.tok.loc.str());
        longjmp(p.jump_err, 1);
    }
    try p.consumeToken();
}

func void Parser.expect(Parser* p, TokenKind k)
{
    if (p.tok.isNot(k))
    {
        sprintf(p.errorMsg, "expected '%s' at %s", token2str(k), p.tok.loc.str());
        longjmp(p.jump_err, 1);
    }
}

const u32 MaxDiag = 128;

public struct TomlReader @opaque
{
    char[MaxDiag] message;
    Blocks* blocks;
}

public func TomlReader* new_toml()
{
    TomlReader* r = @malloc(TomlReader);
    r.blocks = @malloc(Blocks);
    r.blocks.init();
    return r;
}

public func void TomlReader.destroy(TomlReader* r)
{
    r.blocks.destroy();
    free(r.blocks);
    free(r);
}

public func const char* TomlReader.getMsg(const TomlReader* r)
{
    return r.message;
}

error EmptyFileError;

public func void! TomlReader.parse(TomlReader* r, string filename)
{
    Reader file;

    try file.open(filename);

    defer file.close();

    if (file.isEmpty())
    {
        printf("file %s is empty\n", filename);
        raise EmptyFileError;
    }

    Parser parser;
    parser.parse(file.data(), r.message, r.blocks);

$if (DEBUG_NODES)
    r.blocks.dump();
$endif
    return status;
}

// --------------------------------------------------------------
// Getters+iters

func const Node* Reader.findNode(const Reader* r, const char* key)
{
    char[MaxText] name;
    const char* cp = key;
    const char* start = cp;
    u32 len = 0;
    Node* node = nil;
    while (1) {
        switch (*cp) {
        case 0:
            len = cast<u32>(cp - start);
            memcpy(name, start, len);
            name[len] = 0;
            node = r.blocks.findNode(name, node);
            return node;
        case '.':
            len = cast<u32>(cp - start);
            memcpy(name, start, len);
            name[len] = 0;
            start = cp + 1;
            node = r.blocks.findNode(name, node);
            if (!node) return nil;
            if (getKind(node.nameOffset) == NodeKind.Value) return nil;
            break;
        default:
            break;
        }
        cp++;
    }
    return nil;
}

public func const char* Reader.getValue(const Reader* r, const char* key) {
    const Node* node = r.findNode(key);
    if (!node) return nil;
    if (getKind(node.nameOffset) != NodeKind.Value) return nil;
    ValueType t = getRawType(node.rawValue);
    if (t != ValueType.Text) return nil;
    return &r.blocks.values[getRawValue(node.rawValue)];
}

public func bool Reader.getNumber(const Reader* r, const char* key, u32* result) {
    const Node* node = r.findNode(key);
    if (!node) return false;
    if (getKind(node.nameOffset) != NodeKind.Value) return false;
    ValueType t = getRawType(node.rawValue);
    if (t != ValueType.Number) return false;
    *result = getRawValue(node.rawValue);
    return true;
}

public func bool Reader.getBool(const Reader* r, const char* key, bool* result) {
    const Node* node = r.findNode(key);
    if (!node) return false;
    if (getKind(node.nameOffset) != NodeKind.Value) return false;
    ValueType t = getRawType(node.rawValue);
    if (t != ValueType.Boolean) return false;
    *result = getRawValue(node.rawValue);
    return true;
}

public type NodeIter struct {
    const Blocks* blocks;
    const Node* node;
}

public func bool NodeIter.done(const NodeIter* i) {
    return i.node == nil;
}

public func void NodeIter.next(NodeIter* i) {
    if (i.node == nil) return;
    u32 next = i.node.nextNode;
    if (next == 0) i.node = nil;
    else i.node = &i.blocks.nodes[next];
}

public func const char* NodeIter.getValue(const NodeIter* i, const char* key) {
    const Node* child = i.blocks.findNode(key, i.node);
    if (!child) return nil;
    if (getKind(child.nameOffset) != NodeKind.Value) return nil;
    ValueType t = getRawType(child.rawValue);
    if (t != ValueType.Text) return nil;
    return &i.blocks.values[getRawValue(child.rawValue)];
}

public func bool NodeIter.getNumber(const NodeIter* i, const char* key, u32* result) {
    const Node* child = i.blocks.findNode(key, i.node);
    if (!child) return false;
    if (getKind(child.nameOffset) != NodeKind.Value) return false;
    ValueType t = getRawType(child.rawValue);
    if (t != ValueType.Number) return false;
    *result = getRawValue(child.rawValue);
    return true;
}

public func bool NodeIter.getBool(const NodeIter* i, const char* key, bool* result) {
    const Node* child = i.blocks.findNode(key, i.node);
    if (!child) return false;
    if (getKind(child.nameOffset) != NodeKind.Value) return false;
    ValueType t = getRawType(child.rawValue);
    if (t != ValueType.Boolean) return false;
    *result = getRawValue(child.rawValue);
    return true;
}

public func NodeIter Reader.getNodeIter(const Reader* r, const char* key) {
    const Node* node = r.findNode(key);
    if (node && getKind(node.nameOffset) == NodeKind.TableArray) {
        node = &r.blocks.nodes[node.child];
    }
    NodeIter iter = { r.blocks, node}
    return iter;
}


public type ValueIter struct {
    const char* values;
    bool isArray;
}

func ValueIter ValueIter.create(const char* values, bool isArray) {
    ValueIter iter = { values, isArray }
    return iter;
}

public func bool ValueIter.done(const ValueIter* i) {
    return i.values[0] == 0;
}

public func void ValueIter.next(ValueIter* i) {
    if (i.values[0] == 0) return;
    while (i.values[0] != 0) i.values++;
    if (i.isArray) i.values++;    // skip 0-terminator
}

public func const char* ValueIter.getValue(const ValueIter* i) {
    return i.values;
}

public func ValueIter Reader.getValueIter(const Reader* r, const char* key) {
    const Node* node = r.findNode(key);
    if (node) {
        switch (getKind(node.nameOffset)) {
        case NodeKind.Table: fallthrough;
        case NodeKind.TableArray:
            break;
        case NodeKind.ValueArray:
            // TODO support arrays of Numbers/Booleans as well
            return ValueIter.create(&r.blocks.values[getRawValue(node.rawValue)], true);
        case NodeKind.Value:
            return ValueIter.create(&r.blocks.values[getRawValue(node.rawValue)], false);
        case NodeKind.ArrayChild:
            // TODO
            break;
        }
    }
    return ValueIter.create(&r.blocks.values[0], false);
}


// --------------------------------------------------------------
// Blocks

type NodeKind enum u8 {
    Table = 0,
    TableArray,
    ValueArray,
    Value,
    ArrayChild,
}

type ValueType enum u8 {
    Text = 0,
    Number,
    Boolean,
}
const u32 ValueIsArray = (1 << 31);
const u32 ValueTypeOffset = 29;
const u32 RawValueMask = (0x7 << 29);

func const char* type2str(ValueType t) {
    switch (t) {
    case ValueType.Text:    return "T";
    case ValueType.Number:  return "N";
    case ValueType.Boolean: return "B";
    }
    return "";
}


public type Node struct {
    u32 nameOffset;
    u32 nextNode;
    union {
        u32 child;
        u32 rawValue;      // bit 31 isArray, bit 29-30 ValueType
    }
} @(opaque, packed)

public type Blocks struct {
    Node* nodes;
    u32 nodeCount;

    char* names;
    u32 namesOffset;
    u32 namesSize;

    u32[NamesCacheSize] namesCache;
    u32 lastCache;

    char* values;
    u32 valuesOffset;
    u32 valuesSize;
} @(opaque)

func void Blocks.init(Blocks* b) {
    memset(b, 0, sizeof(Blocks));
    b.nodes = calloc(MaxNodes, sizeof(Node));

    b.namesSize = MaxNames;
    b.names = calloc(1, b.namesSize);
    b.names[0] = 0;
    b.namesOffset = 1;  // 0 indicates no name

    b.valuesSize = MaxValues;
    b.values = calloc(1, b.valuesSize);
    b.values[0] = 0;
    b.valuesOffset = 1; // 0 indicates no value

    b.lastCache = 0;
    //memset(b.namesCache, 0, sizeof(b.namesCache));    // sizeof(struct member) not supported yet
    memset(b.namesCache, 0, sizeof(u32)*NamesCacheSize);
}

func void Blocks.destroy(Blocks* b) {
    free(b.values);
    free(b.names);
    free(b.nodes);
}

func u32 Blocks.searchNameCache(Blocks* b, const char* name) {
    for (u32 i=0; i<NamesCacheSize; ++i) {
        u32 off = b.namesCache[i];
        if (off && same(&b.names[off], name)) return off;
    }
    return 0;
}

func const char* Blocks.getName(const Blocks* b, const Node* node) {
    return &b.names[getValue(node.nameOffset)];
}


func u32 Blocks.addNode(Blocks* b, const char* name, NodeKind k) {
    if (b.nodeCount == MaxNodes) {
        // TODO jmp?
        printf("node limit reached\n");
        exit(-1);
    }
    u32 off = b.nodeCount;
    Node* node = &b.nodes[off];
    b.nodeCount++;

    if (name[0] == 0) {
        node.nameOffset = 0;
    } else {
        u32 nameOffset = b.searchNameCache(name);
        if (nameOffset != 0) {
            node.nameOffset = nameOffset;
        } else {
            u32 len = cast<u32>(strlen(name)) + 1;
            nameOffset = b.namesOffset;
            node.nameOffset = nameOffset;
            char* newname = &b.names[nameOffset];
            memcpy(newname, name, len);
            b.namesCache[b.lastCache] = nameOffset;
            b.lastCache = (b.lastCache + 1) % NamesCacheSize;
            b.namesOffset += len;
        }
    }
    node.nameOffset = addKind(node.nameOffset, k);
    return off;
}

func u32 Blocks.addValue(Blocks* b, const char* value) {
    if (value[0] == 0) return 0;
    u32 off = b.valuesOffset;
    u32 len = cast<u32>(strlen(value)) + 1;
    memcpy(&b.values[off], value, len);
    b.valuesOffset += len;
    return off;
}

func void Blocks.addNull(Blocks* b) {
    b.values[b.valuesOffset] = 0;
    b.valuesOffset++;
}

func Node* Blocks.findNode(const Blocks* b, const char* name, const Node* parent) {
    if (b.nodeCount == 0) return nil;
    Node* node = &b.nodes[0];
    if (parent) {
        if (!parent.child) return nil;
        node = &b.nodes[parent.child];
    }
    while (1) {
        const char* nodeName = &b.names[getValue(node.nameOffset)];
        if (same(name, nodeName)) return node;
        if (!node.nextNode) return nil;
        node = &b.nodes[node.nextNode];
    }
    return nil;
}



const u32 NodeKindOffset = 29;

func u32 addKind(u32 value, NodeKind k) @(inline) {
    return value | (k << NodeKindOffset);
}

func NodeKind getKind(u32 value) @(inline) {
    return cast<NodeKind>(value >> NodeKindOffset);
}

func u32 getValue(u32 value) @(inline) {
    return value & ~(0x7 << NodeKindOffset);
}

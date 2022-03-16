// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

static const char *test_parse =
    "struct Node\n"
    "{\n"
    "    uint hole;\n"
    "    uint size;\n"
    "    Node* next;\n"
    "    Node* prev;\n"
    "}\n"
    "\n"
    "struct Footer\n"
    "{ \n"
    "    Node &header;\n"
    "}\n"
    "\n"
    "struct Bin  \n"
    "{\n"
    "    Node& head;\n"
    "}\n"
    "\n"
    "struct Heap  \n"
    "{\n"
    "    size start;\n"
    "    size end;\n"
    "    Bin* bins[BIN_COUNT];\n"
    "}\n"
    "\n"
    "const uint OFFSET = 8;\n"
    "\n"
    "/**\n"
    " * @require start > 0\n"
    " */\n"
    "void Heap.init(Heap& heap, usize start) \n"
    "{\n"
    "    Node& init_region = cast(Node&, start);\n"
    "    init_region.hole = 1;\n"
    "    init_region.size = HEAP_INIT_SIZE - @sizeof(Node) - @sizeof(Footer);\n"
    "\n"
    "    init_region.createFoot();\n"
    "\n"
    "    heap.bins[get_bin_index(init_region.size)].add(init_region);\n"
    "\n"
    "    heap.start = cast(void*, start);\n"
    "    heap.end   = cast(void*, start + HEAP_INIT_SIZE);\n"
    "}\n"
    "\n"
    "void* Heap.alloc(Heap& heap, usize size) \n"
    "{\n"
    "    uint index = get_bin_index(size);\n"
    "    Bin& temp = cast(Bin&, heap.bins[index]);\n"
    "    Node* found = temp.getBestFit(size);\n"
    "\n"
    "    while (!found) \n"
    "    {\n"
    "        temp = heap.bins[++index];\n"
    "        found = temp.getBestFit(size);\n"
    "    }\n"
    "\n"
    "    if ((found.size - size) > (overhead + MIN_ALLOC_SZ)) \n"
    "    {\n"
    "        Node& split = cast(Node*, cast(char&, found) + sizeof(Node) + sizeof(Footer)) + size);\n"
    "        split.size = found.size - size - sizeof(Node) - sizeof(Footer);\n"
    "        split.hole = 1;\n"
    "   \n"
    "        split.createFoot();\n"
    "\n"
    "        uint new_idx = get_bin_index(split.size);\n"
    "\n"
    "        heap.bins[new_idx].addNode(split); \n"
    "\n"
    "        found.size = size; \n"
    "        found.createFoot(found); \n"
    "    }\n"
    "\n"
    "    found.hole = 0; \n"
    "    heap.bins[index].removeNode(found);\n"
    "    \n"
    "    Node& wild = heap.getWilderness(heap);\n"
    "    if (wild.size < MIN_WILDERNESS) \n"
    "    {\n"
    "        uint success = heap.expand(0x1000);\n"
    "        if (success == 0) \n"
    "        {\n"
    "            return null;\n"
    "        }\n"
    "    }\n"
    "    else if (wild.size > MAX_WILDERNESS) \n"
    "    {\n"
    "        heap.contract(0x1000);\n"
    "    }\n"
    "\n"
    "    found.prev = nil;\n"
    "    found.next = nil;\n"
    "    return &found.next; \n"
    "}";
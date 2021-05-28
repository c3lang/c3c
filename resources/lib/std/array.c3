module std::array;
import std::mem;

macro make($Type, usize elements)
{
    assert(elements > 0);
    $Type* ptr = mem::alloc($Type.sizeof, elements);
    return ptr[0..(elements - 1)];
}

macro make_zero($Type, usize elements)
{
    assert(elements > 0);
    $Type* ptr = mem::calloc($Type.sizeof, elements);
    return ptr[0..(elements - 1)];
}

template vararray <Type>
{
    struct List
    {
        usize size;
        usize capacity;
        Type *entries;
    }

    static func void List.ensureCapacity(List *list) @inline
    {
        if (list.capacity == list.size)
        {
            list.capacity = list.capacity ? 2 * list.capacity : 16;
            list.entries = mem::realloc(list.entries, Type.sizeof * list.capacity);
        }
    }

    func void List.push(List *list, Type element) @inline
    {
        list.append(element);
    }

    func void List.append(List *list, Type element)
    {
        list.ensureCapacity();
        list.entries[list.size++] = element;
    }

    /**
     * @require list.size > 0
     */
    func Type List.pop(List *list)
    {
        return list.entries[--list.size];
    }

    /**
     * @require list.size > 0
     */
    func Type List.popFirst(List *list)
    {
        Type value = list.entries[0];
        list.removeAt(0);
        return value;
    }

    func void List.removeAt(List *list, usize index)
    {
        for (usize i = index + 1; i < list.size; i++)
        {
            list.entries[i - 1] = list.entries[i];
        }
        list.size--;
    }

    func void List.pushFront(List *list, Type type) @inline
    {
        list.insertAt(0, type);
    }

    func void List.insertAt(List *list, usize index, Type type)
    {
        list.ensureCapacity();
        for (usize i = list.size; i > index; i--)
        {
            list.entries[i] = list.entries[i - 1];
        }
        list.size++;
        list.entries[index] = type;
    }

    func void List.removeLast(List *list)
    {
        list.size--;
    }

    func void List.removeFirst(List *list)
    {
        list.removeAt(0);
    }

    func Type* List.first(List *list)
    {
        return list.size ? &list.entries[0] : null;
    }

    func Type* List.last(List *list)
    {
        return list.size ? &list.entries[list.size - 1] : null;
    }

    func bool List.isEmpty(List *list)
    {
        return list.size;
    }

    func usize List.len(List *list)
    {
        return list.size;
    }

    func Type List.get(List *list, usize index)
    {
        return list.entries[index];
    }

    func void List.free(List *list)
    {
        mem::free(list.entries);
        list.capacity = 0;
        list.size = 0;
    }
}

template linklist <Type>
{
    static struct Node
    {
        Node *next;
        Node *prev;
        Type value;
    }

    struct LinkedList
    {
        usize size;
        Node *first;
        Node *last;
    }

    func void LinkedList.push(LinkedList *list, Type value)
    {
        list.linkLast(value);
    }

    static func void LinkedList.linkFirst(LinkedList *list, Type value)
    {
        Node *first = list.first;
        Node *new_node = @mem::malloc(Node);
        *new_node = { .next = first, .value = value };
        list.first = new_node;
        if (!first)
        {
            list.last = new_node;
        }
        else
        {
            first.prev = new_node;
        }
        list.size++;
    }

    static func void LinkedList.linkLast(LinkedList *list, Type value)
    {
        Node *last = list.last;
        Node *new_node = mem::alloc(Node.sizeof);
        *new_node = { .prev = last, .value = value };
        list.last = new_node;
        if (!last)
        {
            list.first = new_node;
        }
        else
        {
            last.next = new_node;
        }
        list.size++;
    }

    func void LinkedList.free(LinkedList *list)
    {
        for (Node* node = list.first; node != null;)
        {
            Node* next = node.next;
            mem::free(node);
            node = next;
        }
        list.first = null;
        list.last = null;
        list.size = 0;
    }

    func usize LinkedList.len(LinkedList* list) @inline
    {
        return list.size;
    }

    func Type LinkedList.get(LinkedList* list, usize index)
    {
        Node* node = list.first;
        while (index--)
        {
            node = node.next;
        }
        return node.value;
    }
    /**
     * @require succ != null
     **/
    static func void LinkedList.linkBefore(LinkedList *list, Node *succ, Type value)
    {
        Node* pred = succ.prev;
        Node* new_node = @mem::malloc(Node);
        *new_node = { .prev = pred, .next = succ, .value = value };
        succ.prev = new_node;
        if (!pred)
        {
            list.first = new_node;
        }
        else
        {
            pred.next = new_node;
        }
        list.size++;
    }

    /**
     * @require f == list.first && f != null
     **/
    static func void unlinkFirst(LinkedList* list, Node* f)
    {
        Node* next = f.next;
        mem::free(f);
        list.first = next;
        if (!next)
        {
            list.last = null;
        }
        else
        {
            next.prev = null;
        }
        list.size--;
    }

    /**
     * @require l == list.last && l != null
     **/
    static func void LinkedList.unlinkLast(LinkedList *list, Node* l)
    {
        Node* prev = l.prev;
        list.last = prev;
        mem::free(l);
        if (!prev)
        {
            list.first = null;
        }
        else
        {
            prev.next = null;
        }
        list.size--;
    }

    /**
     * @require x != null
     **/
    static func void LinkedList.unlink(LinkedList* list, Node* x)
    {
        Node* next = x.next;
        Node* prev = x.prev;
        if (!prev)
        {
            list.first = next;
        }
        else
        {
            prev.next = next;
        }
        if (!next)
        {
            list.last = prev;
        }
        else
        {
            next.prev = prev;
        }
        mem::free(x);
        list.size--;
    }
}

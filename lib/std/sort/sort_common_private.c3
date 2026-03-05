module std::sort <Type, CmpFn, Context> @private;

alias ElementType = $typeof(((Type){})[0]);
const bool IS_SLICE = Type.kindof == SLICE;
alias ListType = $typefrom(IS_SLICE ??? Type : Type*);
macro ElementType list_get(ListType l, i) @if(!IS_SLICE) => (*l)[i];
macro ElementType list_get(ListType l, i) @if(IS_SLICE) => l[i];
macro ElementType* list_get_ref(ListType l, i) @if(!IS_SLICE) => &(*l)[i];
macro ElementType* list_get_ref(ListType l, i) @if(IS_SLICE) => &l[i];
macro void list_set(ListType l, i, v) @if(!IS_SLICE) => (*l)[i] = v;
macro void list_set(ListType l, i, v) @if(IS_SLICE) => l[i] = v;
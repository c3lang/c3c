hook global BufCreate .*[.]c3 %{
    set-option buffer filetype c3
}

addhl shared/c3                 regions
addhl shared/c3/code            default-region group
addhl shared/c3/comment-line    region '//' '$' fill comment
addhl shared/c3/comment-block   region /\*  \*/ fill comment
addhl shared/c3/double-string   region 'c?"' (?<!\\)(\\\\)*" fill string
addhl shared/c3/single-string   region "'" (?<!\\)(\\\\)*' fill string

addhl shared/c3/code/           regex '\b(?:extern|break|case|const|continue|default)\b' 0:keyword
addhl shared/c3/code/           regex '\b(?:do|else|for|goto|if|return|sizeof)\b' 0:keyword
addhl shared/c3/code/           regex '\b(?:define|local|errtype|module|as|import)\b' 0:keyword
addhl shared/c3/code/           regex '\b(?:generic|switch|typedef|volatile)\b' 0:keyword
addhl shared/c3/code/           regex '\b(?:while|fn|nil|next|in|$for|$case)\b' 0:keyword
addhl shared/c3/code/           regex '\b(?:%switch|$default|$if|$elif|$else)\b' 0:keyword

addhl shared/c3/code/builtin    regex '($)(\w+)' 1:default 2:function
addhl shared/c3/code/bool       regex '\b(?:true|false)\b' 0:keyword
addhl shared/c3/code/type       regex '\b(?:double|usize|type|Type|bool|char|enum|float|int|uint|long|ulong|short|ushort|struct|void)\b' 0:type
addhl shared/c3/code/operator   regex '(\.|>|<|=|\+|-|\*|/|%|&|^|\||!|:|\?|;|,|@)=?' 0:default
addhl shared/c3/code/num        regex '\b[0-9]+(.[0-9]+)=([eE][+-]?[0-9]+)=' 0:value

hook -group c3-highlight global WinSetOption filetype=c3 %{ add-highlighter window/ ref c3 }
hook -group c3-highlight global WinSetOption filetype=(?!c3).* %{ remove-highlighter window/c3 }

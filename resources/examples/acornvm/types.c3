module acornvm::types;

enum TokenType
{
    LITERAL,    //!< Literal token: null, true, false, int, float, symbol, string
    URL,        //!< Literal url
    NAME,       //!< Named identifier (e.g., for a variable)
    RESERVED,   //!< Reserved word or operator
    EOF         //!< End of file
}

typedef int as AintIdx;
typedef uint as AuintIdx;
typedef byte as AByte;

struct MemCommonInfo
{
	MemInfo* next;      /**< Pointer to next memory block in chain */ \
	AByte enctyp;       /**< Encoding type (see EncType) */ \
	AByte marked;       /**< Garbage collection flags */ \
	AByte flags1;	    /**< Encoding-specific flags */ \
	AByte flags2;	    /**< Encoding-specific flags */ \
	AuintIdx size	    /**< Encoding-specific sizing info */
}

struct MemCommonInfoGray
{
    inline MemCommonInfo;
    MemCommonInfoGray* grayLink;
}

struct MemCommonInfoT
{
    inline MemCommonInfoGray;
    Value type;
}

struct MemInfo
{
    inline MemCommonInfo;
}

struct MemInfoGray
{
    inline MemCommonInfoGray;
}

struct MemInfoT
{
    inline MemCommonInfoT;
}

struct LexInfo
{
	inline MemInfoGray;	        //!< Common header

	Value source;	            //!< The source text
	Value url;		            //!< The url where the source text came from
	Value token;	            //!< Current token
	Value th;		            //!< Current thread

	// Position info
	AuintIdx bytepos;	        //!< Current byte position in source
	AuintIdx linenbr;	        //!< Current line number
	AuintIdx linebeg;	        //!< Beginning of current line
	AuintIdx tokbeg;	        //!< Start of current token in source
	AuintIdx tokline;	        //!< Line number for current token
	AuintIdx toklinepos;        //!< Column position in line for current token

	// indent state
	uint curindent;	            //!< Current level of indentation
	uint newindent;             //!< Indentation level for current line

	int optype;			        //!< sub-type of operator (when type==Op_Token)
	TokenType toktype;	        //!< type of the current token
	bool newline;		        //!< True if we just started a new non-continued line
	bool newprogram;	        //!< True if we have not yet processed any token
	bool insertSemi;	        //!< True if we need to insert ';' as next token
	bool undentcont;	        //!< True if we are processing undenting on a line continuation
}
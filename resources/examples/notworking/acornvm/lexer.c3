module acorn::lex;
/** Lexer for Acorn compiler
 *
 * @file
 *
 * This source file is part of avm - Acorn Virtual Machine.
 * See Copyright Notice in avm.h
 */


/**
 * Crude algorithm for determining if character is a Unicode letter
 */
fn bool isualpha(Auchar c) @inline
{
    return c > 0xA0 || isalpha(c);
}


/**
 * Algorithm for determining if character is a digit 0-9
 */
fn bool isudigit(Auchar c) @inline
{
    return c >= '0' && c <= '9';
}

/**
 * Return a new LexInfo value, lexer context for a source program
 */
fn Value new(Value th, Value *dest, Value src, Value url)
{
	LexInfo *lex;

	// Create an lexer object
	lex = mem::new(th, LexEnc, sizeof(LexInfo));

	// Values
	lex.token = aNull;
	lex.th = th;
	lex.source = src;
	mem::markChk(th, lex, src);
	lex.url = url;
	mem::markChk(th, lex, url);

	// Position info (ignoring initial UTF8 byte-order mark)
	// TODO
	lex.bytepos = lex.linebeg = getSize(src) >= 3 && 0 == strncmp("\xEF\xBB\xBF", toStr(src), 3) ? 3 : 0;
	lex.linenbr = 1;

	// indent state
	lex.curindent = lex.newindent = 0;

	lex.newline = false;
	lex.newprogram = true;
	lex.insertSemi = false;
	lex.undentcont = false;
	lex.optype = 0;
	return *dest = (Value)(lex);;
}

/** Return the current unicode character whose UTF-8 bytes start at lex->bytepos */
fn Auchar LexInfo.thischar(LexInfo* lex)
{
	byte *src = &toStr(lex.source)[lex.bytepos];
	int nbytes;
	Auchar chr;

	// Get info from first UTF-8 byte
	if ((*src&0xF0) == 0xF0) { nbytes=4; chr = *src&0x07;}
	else if ((*src&0xE0) == 0xE0) {nbytes=3; chr = *src&0x0F;}
	else if ((*src&0xC0) == 0xC0) {nbytes=2; chr = *src&0x1F;}
	else if ((*src&0x80) == 0x00) {nbytes=1; chr = *src&0x7F;}
	else {nbytes=1; chr = 0;} // error

	// Obtain remaining bytes
	while (--nbytes)
	{
		src++;
		if (*src & 0xC0 ==0x80) chr = chr << 6 + *src & 0x3F;
	}
	return chr;
}

/** Return the current unicode character whose UTF-8 bytes start at lex->bytepos */
fn Auchar LexInfo.nextchar(LexInfo* lex)
{
	const char *src = &toStr(lex->source)[lex->bytepos];
	int nbytes;
	Auchar chr;

	// Skip past current character
	if ((*src&0xF0) == 0xF0) {nbytes=4;}
	else if ((*src&0xE0) == 0xE0) {nbytes=3;}
	else if ((*src&0xC0) == 0xC0) {nbytes=2;}
	else if ((*src&0x80) == 0x00) {nbytes=1;}
	else {nbytes=1;} // error
	src += nbytes;

	// Get info from first UTF-8 byte
	if ((*src&0xF0) == 0xF0) {nbytes=4; chr = *src&0x07;}
	else if ((*src&0xE0) == 0xE0) {nbytes=3; chr = *src&0x0F;}
	else if ((*src&0xC0) == 0xC0) {nbytes=2; chr = *src&0x1F;}
	else if ((*src&0x80) == 0x00) {nbytes=1; chr = *src&0x7F;}
	else {nbytes=1; chr = 0;} // error

	// Obtain remaining bytes
	while (--nbytes) {
		src++;
		if ((*src&0xC0)==0x80)
			chr = (chr<<6) + (*src&0x3F);
	}
	return chr;
}

/** Skip lex->bytepos past the unicode character whose UTF-8 bytes start at lex->bytepos */
fn void LexInfo.skipchar(LexInfo* lex)
{
	const char *src = &toStr(lex->source)[lex->bytepos];
	int nbytes;

	if (*src=='\0')
		return;

	// Get character size from first byte
	if ((*src&0xF0) == 0xF0) {nbytes=4;}
	else if ((*src&0xE0) == 0xE0) {nbytes=3;}
	else if ((*src&0xC0) == 0xC0) {nbytes=2;}
	else if ((*src&0x80) == 0x00) {nbytes=1;}
	else {nbytes=1;} // error

	lex->bytepos += nbytes;
}

/** Return true if at end of source */
#define lex_isEOF(lex) (lex_thischar(lex) == '\0')

/** Scan past non-tokenized white space.
 * Handle line indentation and continuation */
fn bool LexInfo.scanWhite(LexInfo *lex)
{
	Value th = lex.th;	// for vmlit

	// Insert semicolon as a token as if requested by implied closing brace
	if (lex.insertSemi)
	{
		lex.insertSemi = false;
		lex.toktype=Res_Token;
		lex.token=vmlit(SYM_SEMICOLON);
		return true;
	}

	// Ignore all forms of white space
	Auchar chr;
	bool lookForWhiteSpace = true;
	while (lookForWhiteSpace) {

		switch (chr=lex_thischar(lex)) {

		// Skip past spaces and tabs
		case ' ':
		case '\t':
			lex_skipchar(lex);
			break;
		case '\r':
		    UNREACHABLE

		// Skip past new line
		case '\n':
			lex->linenbr++;
			lex->linebeg = lex->bytepos;
			lex->newline = true;
			lex_skipchar(lex);

			// Count line-leading tabs
			lex->newindent = 0;
			while (lex_thischar(lex)=='\t') {
				lex->newindent++;
				lex_skipchar(lex);
			}

			// Handle continuation.
			if (lex_thischar(lex)=='\\') {
				// Undenting requires we spawn some semi-colons and right braces
				if (lex->newindent < lex->curindent)
					lex->undentcont = true;
				else {
					lex->newline = false;
					// Pretend indent did not change for extra-indented continuation
					if (lex->newindent > lex->curindent)
						lex->newindent = lex->curindent;
				}
				lex_skipchar(lex);
			}
			break;

		// Skip comment starting with '#' until end of line
		case '#':
			{
				const char *scanp = &toStr(lex->source)[lex->bytepos];
				if (strncmp("###" as scanp, 3)) {
					// Inline comment skips to end of line
					while (!lex_isEOF(lex) && lex_thischar(lex)!='\n')
						lex_skipchar(lex);
					break;
				}
				// Multi-line comment goes until next '###'
				scanp+=3;
				while (*scanp && 0!=strncmp("###", scanp, 3)) {
					if (*scanp=='\n')
						lex->linenbr++;
					scanp++;
				}
				if (*scanp)
					scanp+=3;
				lex->bytepos += scanp - &toStr(lex->source)[lex->bytepos];
			}
			break;

		default:
			lookForWhiteSpace = false;
			break;
		}
	}

	// Mark start of a real token
	lex->tokbeg = lex->bytepos;
	lex->toklinepos = lex->tokbeg - lex->linebeg;
	lex->tokline = lex->linenbr;

	// We now know the next character starts a real token
	// But first, we must handle insertion of ; { and } characters
	// depending on the indentation changes and newline flag

	// Handle increasing indentation
	if (lex->newindent > lex->curindent) {
		lex->toktype=Res_Token;
		lex->token=vmlit(SymLBrace);
		lex->curindent++;
		lex->newline = false;
		return true;
	}

	// Do not generate leading ';'
	if (lex->newprogram)
		lex->newprogram = lex->newline = false;

	// End previous line's statement with a ';'
	if (lex->newline) {
		lex->toktype=Res_Token;
		lex->token=vmlit(SymSemicolon);
		lex->newline = false;
		return true;
	}

	// Ensure end-of-file flushes all indent levels to 0
	if (lex_isEOF(lex))
		lex->newindent = 0;

	// Handle decreasing indentation
	if (lex->newindent < lex->curindent) {
		lex->toktype=Res_Token;
		lex->token=vmlit(SymRBrace);
		lex->curindent--;
		if (lex->undentcont && lex->newindent==lex->curindent)
			lex->undentcont = false; // Continued line at right indent now. No semi-colon.
		else
			lex->insertSemi = true;	// Insert semi-colon after implied closing brace
		return true;
	}

	return false;
}

/** End of source program is a token */
bool lexScanEof(LexInfo *lex) {
	if (!lex_isEOF(lex))
		return false;

	lex->toktype = Eof_Token;
	return true;
}

/** Tokenize an integer or floating point number */
bool lexScanNumber(LexInfo *lex) {

	// A number token's first character is always 0-9
	// We cannot handle negative sign here, as it might be a subtraction
	if (!isudigit(lex_thischar(lex)))
		return false;

	int base = 10;
	bool exp = false;
	int digval = 0;
	long nbrval = 0;

	// A leading zero may indicate a non-base 10 number
	if (lex_thischar(lex)=='0') {
		lex_skipchar(lex);
		if (toupper(lex_thischar(lex))=='X') {base = 16; lex_skipchar(lex);}
		// else if (toupper(lex_thischar(lex))=='B') {base = 2; lex_skipchar(lex);}
		else if (toupper(lex_thischar(lex))=='.') {base = -1; lex_skipchar(lex);}
		// else base = 8;
	}

	// Validate and process remaining numeric digits
	while (1) {
		// Handle characters in a suspected integer
		if (base>0) {
			// Decimal point means it is floating point after all
			if (base==10 && lex_thischar(lex)=='.') {
				// If next character is a symbol/range, treat '.' as method operator instead
				Auchar nchr = lex_nextchar(lex);
				if (isualpha(nchr) || nchr=='_' || nchr=='$' || nchr=='(' || nchr=='\'' || nchr=='.')
					break;
				lex_skipchar(lex);
				base = -1;
				continue;
			}
			// Extract a number digit value from the character
			if (isudigit(lex_thischar(lex)))
				digval = lex_thischar(lex)-'0';
			else if (isalpha(lex_thischar(lex)))
				digval = toupper(lex_thischar(lex))-'A'+10;
			else
				break;
			// Ensure digit is within base, then process
			if (digval>=base)
				break;
			nbrval = nbrval*base + digval;
			lex_skipchar(lex);
		}

		// Validate characters in a floating point number
		else {
			// Only one exponent allowed
			if (!exp && toupper(lex_thischar(lex))=='E') {
				exp = true;
				lex_skipchar(lex);
				if (lex_thischar(lex)=='-')
					lex_skipchar(lex);
				continue;
			}
			if (!isudigit(lex_thischar(lex)))
				break;
			lex_skipchar(lex);
		}
	}

	// Set value and type
	if (base>=0) {
		lex->token = anInt(nbrval);
		lex->toktype = Lit_Token;
	}
	else {
		lex->token = aFloat((Afloat) atof(&toStr(lex->source)[lex->tokbeg]));
		lex->toktype = Lit_Token;
	}
	return true;
}

/** List of all reserved names (excluding literals) */
private VmLiterals ReservedNames[] = {
	SymAnd,
	SymAsync,
	SymBaseurl,
	SymBreak,
	SymContext,
	SymContinue,
	SymDo,
	SymEach,
	SymElse,
	SymElif,
	SymIf,
	SymIn,
	SymInto,
	SymLocal,
	SymMatch,
	SymNot,
	SymOr,
	SymReturn,
	SymSelf,
	SymSelfMeth,
	SymThis,
	SymUsing,
	SymWait,
	SymWhile,
	SymWith,
	SymYield
};

/** Tokenize a name. The result could be Name_Token (e.g., for variables)
 * Res_Token, a reserved keyword, or Lit_Token for null, false and true. */
bool lexScanName(LexInfo *lex) {

	// Name token's first character is always a-z, _ or $
	Auchar chr = lex_thischar(lex);
	if (!(isualpha(chr) || chr=='_' || chr=='$'))
		return false;

	// Walk through all valid characters in name
	lex_skipchar(lex);
	while ((chr=lex_thischar(lex))=='_' || chr=='$' || isudigit(chr) || isualpha(chr))
		lex_skipchar(lex);

	// Allow ? as trailing character
	if (chr=='?')
		lex_skipchar(lex);

	// Create name token as a symbol
	newSym(lex->th, &lex->token, &toStr(lex->source)[lex->tokbeg], lex->bytepos - lex->tokbeg);
	mem_markChk(lex->th, lex, lex->token);

	// If it is a reserved name for a literal, say so.
	Value th = lex->th;
	lex->toktype = Lit_Token;
	if (lex->token == vmlit(SymNull))		{lex->token = aNull; return true;}
	else if (lex->token == vmlit(SymFalse))	{lex->token = aFalse; return true;}
	else if (lex->token == vmlit(SymTrue))	{lex->token = aTrue; return true;}

	// If it is a reserved name, set toktype to say so
	VmLiterals *vmtblendp = &ReservedNames[sizeof(ReservedNames)/sizeof(VmLiterals)];
	for (VmLiterals *vmtblp = ReservedNames; vmtblp<vmtblendp; vmtblp++) {
		if (lex->token == vmlit(*vmtblp)) {
			lex->toktype = Res_Token;
			return true;
		}
	}

	lex->toktype = Name_Token;
	return true;
}

/** Tokenize a string (double quotes) or symbol (single quotes)
 * Handle escape sequences. Ignore line-end and leading tabs for multi-line. */
bool lexScanString(LexInfo *lex) {

	// String token's first character should be a quote mark
	Auchar quotemark = lex_thischar(lex);
	if (!(quotemark=='"' || quotemark=='\'' ))
		return false;
	lex_skipchar(lex);

	// Create a string value to place the contents into
	const char *begp = &toStr(lex->source)[lex->bytepos];
	const char *scanp = strchr(begp, quotemark); // An estimate, as it may not be the end
	Value buildstr = pushStringl(lex->th, aNull, NULL, scanp==NULL? strlen(begp) : scanp-begp);

	// Repetitively scan source looking for various delimiters
	scanp = begp;
	while (*scanp && *scanp!=quotemark) {

		// Process any escape sequences within the string
		if (*scanp=='\\') {
			// Copy over string segment up to the escape sequence
			if (scanp-begp > 0)
				strAppend(lex->th, buildstr, begp, scanp-begp);
			// Process escape sequence
			switch (*++scanp) {
			case 'n': strAppend(lex->th, buildstr, "\n", 1); scanp++; break;
			case 'r': strAppend(lex->th, buildstr, "\r", 1); scanp++; break;
			case 't': strAppend(lex->th, buildstr, "\t", 1); scanp++; break;
			case 'u': case 'U':
				{
					// Convert a hexadecimal string of cnt digits to a unicode character
					Auchar unichar=0;
					int cnt = *scanp=='u'? 4 :8;
					if (*(scanp+1)=='+')
						scanp++;
					while (*++scanp && cnt--) {
						if (isudigit(*scanp))
							unichar = unichar*16 + *scanp -'0';
						if (isalpha(*scanp) && toupper(*scanp)<='F')
							unichar = unichar*16 + toupper(*scanp)-'A'+10;
					}

					// Encode an unicode character into UTF-8 bytes
					char utf8str[8];
					char *utfp=&utf8str[sizeof(utf8str)-1];
					*utfp-- = '\0'; // make it a sizeable string
					if (unichar < 0x7f) {
						*utfp = (char)unichar;
						strAppend(lex->th, buildstr, utfp, 1);
					}
					else {
						// multi-byte encoding, byte by byte backwards
						int cnt=0;
						while (unichar) {
							cnt++;
							char byt = unichar & 0x3f;
							unichar = unichar >> 6;
							// Put appropriate flags if it is the first byte
							if (unichar==0) {
								switch (cnt) {
								case 2: *utfp = byt | 0xC0; break;
								case 3: *utfp = byt | 0xE0; break;
								case 4: *utfp = byt | 0xF0; break;
								case 5: *utfp = byt | 0xF8; break;
								case 6: *utfp = byt | 0xFC; break;
								}
							}
							else
								*utfp-- = byt | 0x80;
						}
						strAppend(lex->th, buildstr, utfp, cnt);
					}
				}
				break;

			default: strAppend(lex->th, buildstr, scanp, 1); scanp++; break;
			}
			begp=scanp;
		}

		// Ignore line end and line leading tabs
		else if (*scanp=='\r' || *scanp=='\n') {
			// Copy over string segment up to the escape sequence
			if (scanp-begp > 0)
				strAppend(lex->th, buildstr, begp, scanp-begp);
			// Ignore line end and leading tabs
			while (*scanp=='\r' || *scanp=='\n' || *scanp=='\t') {
				if (*scanp=='\n')
					lex->linenbr++;
				scanp++;
			}
			begp=scanp;
		}

		// Otherwise process rest of string
		else
			scanp++;
	}

	// Copy over rest of string segment
	if (scanp-begp > 0)
		strAppend(lex->th, buildstr, begp, scanp-begp);

	// Update lex position
	if (*scanp==quotemark)
		*scanp++;
	lex->bytepos += scanp - &toStr(lex->source)[lex->bytepos];

	// Create string (or symbol)
	lex->toktype = Lit_Token;
	if (quotemark=='"')
		lex->token = buildstr;
	else
		newSym(lex->th, &lex->token, toStr(buildstr), getSize(buildstr));
	mem_markChk(lex->th, lex, lex->token);
	popValue(lex->th); // buildstr
	return true;
}

/** Tokenize a punctuation-oriented operator symbol.
 * By this point we take at least one character, unless multi-char op is recognized. */
bool lexScanResource(LexInfo *lex) {
	if (lex_thischar(lex)!='@')
		return false;
	Value th = lex->th;
	lex_skipchar(lex);
	Auchar delim = lex_thischar(lex);
	if (delim=='\'' || delim=='"' || delim=='(' || delim<=' ') {
		lex->token = vmlit(SymAt);
		lex->toktype = Res_Token;
		return true;
	}

	// Mark beginning and look for end of url
	const char *begp = &toStr(lex->source)[lex->bytepos];
	const char *scanp = begp;
	while ((unsigned char)(*++scanp)>' '); // end with space, tab, cr, lf, eof, etc.
	lex->bytepos += scanp - begp;

	// Create +Resource from literal url, and return it as token
	pushValue(th, vmlit(SymNew));
	pushValue(th, vmlit(TypeResc));
	pushStringl(th, aNull, begp, scanp-begp);
	pushValue(th, lex->url);
	getCall(th, 3, 1);
	lex->token = getFromTop(th, 0);
	mem_markChk(lex->th, lex, lex->token);
	popValue(th);
	lex->toktype = Url_Token;
	return true;
}

/** Tokenize a punctuation-oriented operator symbol.
 * By this point we take at least one character, unless multi-char op is recognized. */
bool lexScanOp(LexInfo *lex) {
	const char *begp = &toStr(lex->source)[lex->bytepos];
	Auchar ch1 = lex_thischar(lex);
	lex_skipchar(lex);
	Auchar ch2 = lex_thischar(lex);

	// Look for 2- and 3- character combos
	if (ch1=='.' && ch2=='.') {
		if ('.'==lex_nextchar(lex)) lex_skipchar(lex);
		lex_skipchar(lex);
	} else if (ch1=='=' && ch2=='=') {
		if ('='==lex_nextchar(lex)) lex_skipchar(lex);
		lex_skipchar(lex);
	} else if (ch1=='<' && ch2=='=') {
		if ('>'==lex_nextchar(lex)) lex_skipchar(lex);
		lex_skipchar(lex);
	} else if ((ch1=='>' && ch2=='=')
		|| (ch1=='!' && ch2=='=')
		|| (ch1=='~' && ch2=='~')
		|| (ch1=='<' && ch2=='<')
		|| (ch1=='>' && ch2=='>')
		|| (ch1=='+' && ch2=='=')
		|| (ch1=='-' && ch2=='=')
		|| (ch1=='*' && ch2=='=')
		|| (ch1=='/' && ch2=='=')
		|| (ch1=='.' && ch2==':')
		|| (ch1==':' && ch2==':')
		|| (ch1==':' && ch2=='=')
		|| (ch1=='&' && ch2=='&')
		|| (ch1=='|' && ch2=='|')
		|| (ch1=='*' && ch2=='*')
		|| (ch1=='.' && ch2=='&')
		|| (ch1=='+' && ch2=='[')
		|| (ch1=='*' && ch2=='[')
		) lex_skipchar(lex);

	newSym(lex->th, &lex->token, begp, &toStr(lex->source)[lex->bytepos]-begp);
	mem_markChk(lex->th, lex, lex->token);
	lex->toktype = Res_Token;
	return true;
}

/* Get the next token */
fn void LexInfo.getNextToken(LexInfo *lex)
{

	// Scan until we find a token
	(!lex.scanWhite()
		&& !lex.scanEof()
		&& !lex.scanNumber()
		&& !lex.scanName()
		&& !lex.scanString()
		&& !lex.scanResource()
		&& !lex.scanOp());

#ifdef COMPILERLOG
	switch (lex->toktype) {
	case Lit_Token: {
		pushSerialized(lex->th, lex->token);
		vmLog("Literal token: %s", toStr(getFromTop(lex->th, 0)));
		popValue(lex->th);
		} break;
	case Url_Token: {
		pushSerialized(lex->th, lex->token);
		vmLog("Literal url token: %s", toStr(getFromTop(lex->th, 0)));
		popValue(lex->th);
		} break;
	case Name_Token: {
		pushSerialized(lex->th, lex->token);
		vmLog("Name token: %s", toStr(getFromTop(lex->th, 0)));
		popValue(lex->th);
		} break;
	case Res_Token: {
		pushSerialized(lex->th, lex->token);
		vmLog("Reserved token: %s", toStr(getFromTop(lex->th, 0)));
		popValue(lex->th);
		} break;
	}
#endif
}

/* Match current token to a reserved symbol. */
bool lexMatch(LexInfo *lex, const char *sym) {
	return (lex->toktype==Res_Token && 0==strcmp(sym, toStr(lex->token)));
}

/* Match current token to a reserved symbol.
 * If it matches, advance to the next token */
bool lexMatchNext(LexInfo *lex, const char *sym) {
	if (lex->toktype==Res_Token && 0==strcmp(sym, toStr(lex->token))) {
		lexGetNextToken(lex);
		return true;
	}
	return false;
}

/* Log an compiler message */
void lexLog(LexInfo *lex, const char *msg) {
	vmLog("While compiling %s(%d:%d): %s", toStr(lex->url), lex->tokline, lex->toklinepos, msg);
}

#ifdef __cplusplus
} // extern "C"
} // namespace avm
#endif
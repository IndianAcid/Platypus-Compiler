/*******************************************************************************
File name:			scanner.c
Compiler:			Microsoft Visual Studio 2015
Author:				Svillen Ranev
Hasan Skaiky, 040 ### ###
Mohamed Elmekki, 040 ### ###
Course:				CST 8152 – Compilers, Lab Section: 013
Assignment:			2
Date:				November 30, 2017
Professor:			Svillen Ranev
Purpose:			Implementation of the functionality of a lexical analyzer.
To generate tokens based on the grammatic definition of the
Platypus language
*******************************************************************************/

/* The #define _CRT_SECURE_NO_WARNINGS should be used in MS Visual Studio projects
* to suppress the warnings about using "unsafe" functions like fopen()
* and standard sting library functions defined in string.h.
* The define does not have any effect in Borland compiler projects.
*/
#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>   /* standard input / output */
#include <ctype.h>   /* conversion functions */
#include <stdlib.h>  /* standard library functions and constants */
#include <string.h>  /* string functions */
#include <limits.h>  /* integer types constants */
#include <float.h>   /* floating-point types constants */

/*#define NDEBUG        to suppress assert() call */
#include <assert.h>  /* assert() prototype */

/* project header files */
#include "buffer.h"
#include "token.h"
#include "table.h"

#define DEBUG  /* for conditional processing */
#undef  DEBUG

/* The minimum/maximum values for a 2-byte unsigned integer */
#define PLS_INL_MAX 32767

/* Global objects - variables */
/* This buffer is used as a repository for string literals.
It is defined in platy_st.c */
extern Buffer * str_LTBL; /*String literal table */
int line; /* current line number of the source code */
extern int scerrnum;     /* defined in platy_st.c - run-time error number */

						 /* Local(file) global objects - variables */
static Buffer *lex_buf;/*pointer to temporary lexeme buffer*/

					   /* No other global variable declarations/definitiond are allowed */

					   /* scanner.c static(local) function  prototypes */
static int char_class(char c); /* character class function */
static int get_next_state(int, char, int *); /* state machine function */
static long atolh(char * lexeme); /* converts hexadecimal string to decimal value */

								  /*Initializes scanner */
int scanner_init(Buffer * sc_buf) {
	if (b_isempty(sc_buf)) return EXIT_FAILURE;/*1*/
											   /* in case the buffer has been read previously  */
	b_rewind(sc_buf);
	b_clear(str_LTBL);
	line = 1;
	return EXIT_SUCCESS;/*0*/
						/*   scerrnum = 0;  *//*no need - global ANSI C */
}

/*******************************************************************************
Purpose:			This buffer attempts to add the given char argument to the
character buffer
Author:				Svillen Ranev, Hasan Skaiky, Mohamed Elmekki
History/Versions:	1.0
Called functions:	b_getc()
b_retract()
b_mark()
b_getcoffset()
b_addc()
b_reset()
Parameters:			Buffer * sc_buf
Pointer to the buffer from which to analyze
Return value:		Token
A completed token for each lexeme found in the buffer
Algorithm:			Traverse through the buffer
Determine the state corresponding to each character
*******************************************************************************/
Token malar_next_token(Buffer * sc_buf) {
	Token t; /* token to return after recognition */
	unsigned char c; /* input symbol */
	int state = 0; /* initial state of the FSM */
	short lexstart;  /*start offset of a lexeme in the input char buffer (array) */
	short lexend;    /*end offset of a lexeme in the input char buffer (array)*/
	int accept = NOAS; /* type of state - initially not accepting */

	int i = 0;

	while (1) { /* endless loop broken by token returns it will generate a warning */

		c = b_getc(sc_buf);

		/* special cases or token driven processing */

		switch (c) {

			/*PROCESSING INCONSEQUENTIAL CHARACTERS*/

			/*White space*/
		case ' ':
			/*Tab*/
		case '\t':
			/*Form feed*/
		case '\f':
			/*Vertical Tab*/
		case '\v':
			continue;

			/*Carriage return*/
		case '\r':
			line++;
			continue;

			/*New line character*/
		case '\n':
			line++;
			continue;

			/*Single-line comments*/
		case '!':
			c = b_getc(sc_buf);

			/* In the case of two consecutive '!', process a comment */
			if (c == '!') {
				/* Consume all following characters from buffer */
				while (c != '\n' && c != '\0' && c != 255 && c != '\r') {
					c = b_getc(sc_buf);
				}

				line++;
				continue;
			}
			else {
				/* If second '!' is absent, generate an error token */
				t.code = ERR_T;
				t.attribute.err_lex[0] = '!';
				t.attribute.err_lex[1] = c;
				t.attribute.err_lex[2] = '\0';

				while (1) {
					c = b_getc(sc_buf);

					if (c == '\n' || c == '\0' || c == 255 || c == '\r') {
						break;
					}
				}
				line++;
			}

			return t;

			/*PROCESSING SPECIAL CHARACTERS */

		case '{':
			t.code = LBR_T;
			return t;
		case '}':
			t.code = RBR_T;
			return t;
		case '(':
			t.code = LPR_T;
			return t;
		case ')':
			t.code = RPR_T;
			return t;
		case ',':
			t.code = COM_T;
			return t;
		case ';':
			t.code = EOS_T;
			return t;

			/*End of file */
		case EOF:
			t.code = SEOF_T;
			return t;

		case '\0':
			t.code = SEOF_T;
			return t;

		case 0xFF:
			t.code = SEOF_T;
			return t;

			/*String concatenation character*/
		case '#':
			t.code = SCC_OP_T;
			return t;

			/*Arithmetic operators*/
		case '+':
			t.code = ART_OP_T;
			t.attribute.arr_op = PLUS;
			return t;
		case '-':
			t.code = ART_OP_T;
			t.attribute.arr_op = MINUS;
			return t;
		case '*':
			t.code = ART_OP_T;
			t.attribute.arr_op = MULT;
			return t;
		case '/':
			t.code = ART_OP_T;
			t.attribute.arr_op = DIV;
			return t;

			/* Relational Operators*/
		case '=':
			c = b_getc(sc_buf);
			/* If '=' stands alone, register an assignment operator.
			Otherwise, register a "equal to" relation operator */
			if (c == '=') {
				t.code = REL_OP_T;
				t.attribute.rel_op = EQ;
			}
			else {
				b_retract(sc_buf);
				t.code = ASS_OP_T;
			}
			return t;

		case '<':
			/* Register a "less than" relational operator, or a "not equal to"
			relational operator */
			if (b_getc(sc_buf) == '>') {
				t.code = REL_OP_T;
				t.attribute.rel_op = NE;
				return t;
			}

			b_retract(sc_buf);
			t.code = REL_OP_T;
			t.attribute.rel_op = LT;
			return t;

		case '>':
			/* Register a "greater than" operator */
			t.code = REL_OP_T;
			t.attribute.rel_op = GT;
			return t;

			/*Logical operators*/
		case '.':

			/* If, following a period, lexeme matches language grammar for
			logical operators, register respective operator */
			b_mark(sc_buf, b_getcoffset(sc_buf));
			c = b_getc(sc_buf);

			if (c == 'A') {
				c = b_getc(sc_buf);

				if (c == 'N') {
					c = b_getc(sc_buf);

					if (c == 'D') {
						c = b_getc(sc_buf);

						if (c == '.') {
							t.code = LOG_OP_T;
							t.attribute.log_op = AND;

							return t;
						}
					}
				}
			}
			else if (c == 'O') {
				c = b_getc(sc_buf);

				if (c == 'R') {
					c = b_getc(sc_buf);

					if (c == '.') {
						t.code = LOG_OP_T;
						t.attribute.log_op = OR;

						return t;
					}
				}
			}

			/* Generate an error token if above fails */
			t.code = ERR_T;
			t.attribute.err_lex[0] = '.';
			t.attribute.err_lex[1] = '\0';

			b_reset(sc_buf);

			return t;

			/* Literal strings*/
		case '"':
			/* Following a double-quote, seek for the matching double-quote.
			If absent, generate an error token*/
			b_mark(sc_buf, b_getcoffset(sc_buf));
			lexstart = b_getcoffset(sc_buf);

			while (1) {
				c = b_getc(sc_buf);

				if (c == '"') {
					lexend = b_getcoffset(sc_buf);
					b_reset(sc_buf);

					t.attribute.str_offset = b_limit(str_LTBL);

					for (i = lexstart; i < (lexend - 1); i++) {
						c = b_getc(sc_buf);
						b_addc(str_LTBL, c);
					}

					b_getc(sc_buf);

					b_addc(str_LTBL, '\0');
					t.code = STR_T;

					return t;
				}
				else if (c == '\0' || c == 255 || c == EOF) {
					b_reset(sc_buf);
					b_retract(sc_buf);

					for (i = 0; i < (ERR_LEN - 3); i++) {
						c = b_getc(sc_buf);

						if (c == '\n' || c == '\0' || c == 255) {
							break;
						}

						t.attribute.err_lex[i] = c;
					}

					t.code = ERR_T;
					t.attribute.err_lex[i++] = '.';
					t.attribute.err_lex[i++] = '.';
					t.attribute.err_lex[i++] = '.';
					t.attribute.err_lex[i++] = '\0';

					c = b_getc(sc_buf);

					while (c != '\0' && c != 255 && c != EOF) {
						c = b_getc(sc_buf);
					}

					return t;
				}
				else if (c == '\n') {
					line++;
				}
			}

			/* Variable identifier */
		default:
			/* If first character of a lexeme begins a VID, process lexeme as
			per Platypus transition table */
			if (isalpha(c) || isdigit(c)) {
				lexstart = b_mark(sc_buf, b_getcoffset(sc_buf) - 1);
				state = get_next_state(state, c, &accept);

				while (accept == NOAS) {
					c = b_getc(sc_buf);
					state = get_next_state(state, c, &accept);
				}

				if (accept == ASWR) {
					b_retract(sc_buf);
				}

				lexend = b_getcoffset(sc_buf);

				b_retract(sc_buf);

				lex_buf = b_allocate((lexend - lexstart) + 1, 0, 'f');

				if (!lex_buf) {
					t.code = ERR_T;
					strcpy(t.attribute.err_lex, lex_buf->cb_head);
					return t;
				}

				b_reset(sc_buf);

				for (i = 0; i < (lexend - lexstart); i++) {
					c = b_getc(sc_buf);
					b_addc(lex_buf, c);
				}

				b_addc(lex_buf, '\0');

				t = aa_table[state](lex_buf->cb_head);

				free(lex_buf);

				return t;
			}

			/* In case of unknown/unexpected character, generate error token */
			t.code = ERR_T;
			t.attribute.err_lex[0] = c;
			t.attribute.err_lex[1] = '\0';

			return t;
		}
	}
}

/*******************************************************************************
Purpose:			After receiving a character, this function determines the
successive state, given the previous state
Author:				Svillen Ranev
History/Versions:	1.0
Called functions:	char_class()
printf()
Parameters:			int state
The state preceding the given character
int next
The next state, determined by the transition table and
given the next character
int * accept
The current state; updated within this function
Return value:		int
The next state, after referring to the transition table
Algorithm:			Cross-reference initial state with the next character
*******************************************************************************/
int get_next_state(int state, char c, int *accept) {
	int col;
	int next;
	/* The class of the given character, corresponding to the transition table
	columns */
	col = char_class(c);
	/* The successive state */
	next = st_table[state][col];
#ifdef DEBUG
	printf("Input symbol: %c Row: %d Column: %d Next: %d \n", c, state, col, next);
#endif
	/* For the purpose of run-time diagnosis */
	assert(next != IS);

#ifdef DEBUG
	if (next == IS) {
		printf("Scanner Error: Illegal state:\n");
		printf("Input symbol: %c Row: %d Column: %d\n", c, state, col);
		exit(1);
	}
#endif
	*accept = as_table[next];
	return next;
}

/*******************************************************************************
Purpose:			To classify the given character given the categories in the
transition table
Author:				Hasan Skaiky, Mohamed Elmekki
History/Versions:	1.0
Called functions:	N/A
Parameters:			char c
The character to classify
Return value:		int
The given character's class
Algorithm:
*******************************************************************************/
int char_class(char c) {

	if (c == 'x') {
		return 0;
	}
	else if ((c >= 'a' && c <= 'w') || (c >= 'y' && c <= 'z')) {
		return 1;
	}
	else if (c >= 'A' && c <= 'F') {
		return 2;
	}
	else if (c >= 'G' && c <= 'Z') {
		return 3;
	}
	else if (c == '0') {
		return 4;
	}
	else if (c >= '1' && c <= '9') {
		return 5;
	}
	else if (c == '.') {
		return 6;
	}
	else if (c == '$') {
		return 7;
	}
	else {
		return 8;
	}

}

/*******************************************************************************
Purpose:			To generate an AVID or KEYWORD token from the given lexeme
Author:				Hasan Skaiky, Mohamed Elmekki
History/Versions:	1.0
Called functions:	strlen()
Parameters:			char * lexeme
A lexeme predefined as an AVID or KEYWORD
Return value:		Token
A fully constructor AVID or KEYWORD token
Algorithm:			Cross reference lexeme against known keywords and generate a
KEYWORD token
If not a keyword, generate an AVID token
*******************************************************************************/
Token aa_func02(char * lexeme) {

	Token t;
	int i = 0;
	int lexeme_length;

	for (i = 0; i < KWT_SIZE; i++) {
		if (strcmp(lexeme, kw_table[i]) == 0) {
			t.code = KW_T;
			t.attribute.kwt_idx = i;
			return t;
		}
	}

	t.code = AVID_T;

	i = 0;
	lexeme_length = strlen(lexeme);

	while (i < lexeme_length && i < VID_LEN) {
		t.attribute.vid_lex[i] = lexeme[i];
		i++;
	}

	t.attribute.vid_lex[i] = '\0';

	return t;

}

/*******************************************************************************
Purpose:			To generate an SVID token
Author:				Hasan Skaiky, Mohamed Elmekki
History/Versions:	1.0
Called functions:	strlen()
Parameters:			char * lexeme
A lexeme predefined as an SVID
Return value:		Token
A fully constructor SVID token
Algorithm:			Generate token; control token length
*******************************************************************************/
Token aa_func03(char * lexeme) {

	Token t;
	int i = 0;
	int lexeme_length = strlen(lexeme);

	t.code = SVID_T;

	if (lexeme_length > VID_LEN) {
		while (i < lexeme_length && i < VID_LEN - 1) {
			t.attribute.vid_lex[i] = lexeme[i];
			i++;
		}

		t.attribute.vid_lex[i++] = '$';
		t.attribute.vid_lex[i] = '\0';
	}
	else {
		while (i < lexeme_length) {
			t.attribute.vid_lex[i] = lexeme[i];
			i++;
		}

		t.attribute.vid_lex[i] = '\0';
	}

	return t;

}

/*******************************************************************************
Purpose:			To generate an INL_T token, given an integer literal
Author:				Hasan Skaiky, Mohamed Elmekki
History/Versions:	1.0
Called functions:	atoi()
aa_func12()
Parameters:			char * lexeme
A lexeme predefined as an integer literal
Return value:		Token
A fully constructor INL_T token
Algorithm:			Generate token; ensure resulting integer does not exceed
2-byte capacity
*******************************************************************************/
Token aa_func05(char * lexeme) {

	Token t;
	long int_value;

	if (strlen(lexeme) > INL_LEN) {
		return aa_func12(lexeme);
	}

	int_value = atol(lexeme);

	if (int_value >= 0 && int_value <= PLS_INL_MAX) {
		t.code = INL_T;
		t.attribute.int_value = int_value;
		return t;
	}

	t = aa_func12(lexeme);

	return t;

}

/*******************************************************************************
Purpose:			To generate an FPL_T token, given a floating-point literal
Author:				Hasan Skaiky, Mohamed Elmekki
History/Versions:	1.0
Called functions:	atof()
aa_func12()
Parameters:			char * lexeme
A lexeme predefined as an floating-point literal
Return value:		Token
A fully constructor FPL_T token
Algorithm:			Generate token; ensure resulting floating-point value does
not exceed 4-byte capacity
*******************************************************************************/
Token aa_func08(char * lexeme) {

	Token t;
	double float_value = (double)atof(lexeme);

	if ((float_value >= FLT_MIN && float_value <= FLT_MAX) || float_value == 0) {
		t.code = FPL_T;
		t.attribute.flt_value = (float)float_value;
		return t;
	}

	t = aa_func12(lexeme);

	return t;

}

/*******************************************************************************
Purpose:			To generate an INL_T token, given a hexadecimal literal
Author:				Hasan Skaiky, Mohamed Elmekki
History/Versions:	1.0
Called functions:	atolh()
aa_func12()
Parameters:			char * lexeme
A lexeme predefined as an hexadecimal literal
Return value:		Token
A fully constructor INL_T token
Algorithm:			Generate token; ensure resulting integer does not exceed
2-byte capacity
*******************************************************************************/
Token aa_func11(char * lexeme) {

	Token t;
	long hex_value = atolh(lexeme);

	if (hex_value >= 0 && hex_value < PLS_INL_MAX) {
		t.code = INL_T;
		t.attribute.int_value = hex_value;
		return t;
	}

	t = aa_func12(lexeme);

	return t;

}

/*******************************************************************************
Purpose:			To generate an ERR_T
Author:				Hasan Skaiky, Mohamed Elmekki
History/Versions:	1.0
Called functions:	strlen()
Parameters:			char * lexeme
A lexeme predefined as an erroneous lexeme
Return value:		Token
A fully constructor ERR_T token
Algorithm:			Generate an error token; control length of token attribute
*******************************************************************************/
Token aa_func12(char * lexeme) {

	Token t;
	int i = 0;
	int max_length = 0;
	int lexeme_length = strlen(lexeme);

	t.code = ERR_T;

	if (lexeme_length > 20) {
		max_length = ERR_LEN - 3;

		while (i < max_length) {
			t.attribute.err_lex[i] = lexeme[i];
			i++;
		}

		for (i; i < ERR_LEN; i++) {
			t.attribute.err_lex[i] = '.';
		}

		t.attribute.err_lex[i] = '\0';
	}
	else {
		max_length = lexeme_length;

		while (i < max_length) {
			t.attribute.err_lex[i] = lexeme[i];
			i++;
		}

		t.attribute.err_lex[i] = '\0';
	}

	return t;

}

/*******************************************************************************
Purpose:			To convert a given hexadecimal literal into a long integer
Author:				Hasan Skaiky, Mohamed Elmekki
History/Versions:	1.0
Called functions:	strtol()
Parameters:			char * lexeme
A lexeme predefined as a hexadecimal literal
Return value:		long
The given hexadecimal expressed as a long integer
Algorithm:
*******************************************************************************/
long atolh(char * lexeme) {

	return strtol(lexeme, NULL, 16);

}
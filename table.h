/*******************************************************************************
File name:			table.h
Compiler:			Microsoft Visual Studio 2015
Author:				Svillen Ranev
Hasan Skaiky, 040 ### ###
Mohamed Elmekki, 040 ### ###
Course:				CST 8152 – Compilers, Lab Section: 013
Assignment:			2
Date:				September 29, 2017
Professor:			Svillen Ranev
Purpose:			Definitions of the transition table used to determine
grammatic correctness of given lexemes
Declarations of functions that ultimately generate tokens
*******************************************************************************/

#ifndef  TABLE_H_
#define  TABLE_H_ 

#ifndef BUFFER_H_
#include "buffer.h"
#endif

#ifndef NULL
#include <_null.h> /* NULL pointer constant is defined there */
#endif

#define IS -1 /* Invalid state */
#define ES 12 /* Error state - No retract */
#define ER 13 /* Error state - Retract*/

/* State transition table definition */
#define TABLE_COLUMNS 9
#define TABLE_ROWS 14
/*transition table - type of states defined in separate table */
int  st_table[TABLE_ROWS][TABLE_COLUMNS] = {
	/* State 0 */{ 1,  1,  1,  1,  6,  4,  IS, IS, IS },
	/* State 1 */{ 1,  1,  1,  1,  1,  1,  2,  3,  2 },
	/* State 2 */{ IS, IS, IS, IS, IS, IS, IS, IS, IS },
	/* State 3 */{ IS, IS, IS, IS, IS, IS, IS, IS, IS },
	/* State 4 */{ ES, ES, ES, ES, 4,  4,  7,  5,  5 },
	/* State 5 */{ IS, IS, IS, IS, IS, IS, IS, IS, IS },
	/* State 6 */{ 9,  ES, ES, ES, 5,  ES, 7,  ER, 5 },
	/* State 7 */{ 8,  8,  8,  8,  7,  7,  8,  8,  8 },
	/* State 8 */{ IS, IS, IS, IS, IS, IS, IS, IS, IS },
	/* State 9 */{ ER, ER, 10, ER, 10, 10, ER, ER, ER },
	/* State 10 */{ ES, ES, 10, ES, 10, 10, 11, 11, 11 },
	/* State 11 */{ IS, IS, IS, IS, IS, IS, IS, IS, IS },
	/* State 12 */{ IS, IS, IS, IS, IS, IS, IS, IS, IS },
	/* State 13 */{ IS, IS, IS, IS, IS, IS, IS, IS, IS }
};

/* Accepting state table definition */
#define ASWR     00  /* accepting state with retract */
#define ASNR     01  /* accepting state with no retract */
#define NOAS     02  /* not accepting state */

int as_table[] = {
	NOAS, NOAS, ASWR, ASNR, NOAS, ASWR, NOAS,
	NOAS, ASWR, NOAS, NOAS, ASWR, ASNR, ASWR
};

/* Accepting action function declarations */

/* Accepting state with retract - AVID*/
Token aa_func02(char * lexeme);
/* Accepting state without retract - SVID */
Token aa_func03(char * lexeme);
/* Accepting state with retract - DIL/0 */
Token aa_func05(char * lexeme);
/* Accepting state with retract - FPL */
Token aa_func08(char * lexeme);
/* Accepting state with retract - HIL */
Token aa_func11(char * lexeme);
/* Accepting state with retract - ER */
Token aa_func12(char * lexeme);

/*
defining a new type: pointer to function (of one char * argument)
returning Token
*/

typedef Token(*PTR_AAF)(char *lexeme);

/* Accepting function (action) callback table (array) definition */
/* If you do not want to use the typedef, the equivalent declaration is:
* Token (*aa_table[])(char lexeme[]) = {
*/

PTR_AAF aa_table[TABLE_ROWS] = {
	NULL,
	NULL,
	aa_func02,
	aa_func03,
	NULL,
	aa_func05,
	NULL,
	NULL,
	aa_func08,
	NULL,
	NULL,
	aa_func11,
	aa_func12,
	aa_func12
};

/* Keyword lookup table (.AND. and .OR. are not keywords) */

#define KWT_SIZE  10

char * kw_table[KWT_SIZE] = {
	"ELSE",
	"FALSE",
	"IF",
	"PLATYPUS",
	"READ",
	"REPEAT",
	"THEN",
	"TRUE",
	"WHILE",
	"WRITE"
};

#endif

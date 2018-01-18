/*******************************************************************************
File name:			buffer.h
Compiler:			Microsoft Visual Studio 2015
Author:				Hasan Skaiky, 040 672 444
Mohamed Elmekki, 040 847 947
Course:				CST 8152 – Compilers, Lab Section: 013
Assignment:			2
Date:				September 29, 2017
Professor:			Svillen Ranev
Purpose:			To declare and define attributes for the Buffer structure.
To declare the functions implemented and used throughout
the Buffer's lifespan.
*******************************************************************************/
#ifndef BUFFER_H_
#define BUFFER_H_

/*#pragma warning(1:4001) */
/*to enforce C89 type comments  - to make //comments an warning */

/*#pragma warning(error:4001)*/
/* to enforce C89 comments - to make // comments an error */

/* standard header files */
#include <stdio.h>  /* standard input/output */
#include <malloc.h> /* for dynamic memory allocation*/
#include <limits.h> /* implementation-defined data type ranges and limits */

/* constant definitions */
/* You may add your own constant definitions here */
#define RT_FAIL1 -1         /* fail return value */
#define RT_FAIL2 -2         /* fail return value */
#define LOAD_FAIL -2       /* load fail error */
#define SET_R_FLAG 1       /* realloc flag set value */

#define FIXED_MODE 0					/* indicates fixed-mode buffer */
#define ADD_MODE 1						/* indicates additive-mode buffer */
#define MULTI_MODE -1					/* indicates multiplicative-mode buffer */

#define ZERO_INIT_CAPACITY 0			/* an invalid init_capacity value */
#define MAX_INIT_CAPACITY SHRT_MAX - 1	/* an invalid init_capacity_value */

#define FIXED_INC_FACTOR 0				/* the default inc_factor for a fixed-mode buffer */
#define ADD_MODE_MAX_INC_FACTOR 255		/* maximum allowed inc_factor for additive mode */
#define MULTI_MODE_MAX_INC_FACTOR 100	/* maximum allowed inc_factor for multiplicative mode */
#define INVALID_INC_FACTOR 256			/* an invalid inc_factor value */

/* user data type declarations */
typedef struct BufferDescriptor {
	char *cb_head;   /* pointer to the beginning of character array (character buffer) */
	short capacity;    /* current dynamic memory size (in bytes) allocated to character buffer */
	short addc_offset;  /* the offset (in chars) to the add-character location */
	short getc_offset;  /* the offset (in chars) to the get-character location */
	short markc_offset; /* the offset (in chars) to the mark location */
	char  inc_factor; /* character array increment factor */
	char  r_flag;     /* reallocation flag */
	char  mode;       /* operational mode indicator*/
	int   eob;       /* end-of-buffer flag */
} Buffer, *pBuffer;


/* function declarations */
Buffer * b_allocate(short init_capacity, char inc_factor, char o_mode);
pBuffer b_addc(pBuffer const pBD, char symbol);
int b_clear(Buffer * const pBD);
void b_free(Buffer * const pBD);
int b_isfull(Buffer * const pBD);
short b_limit(Buffer * const pBD);
short b_capacity(Buffer * const pBD);
short b_mark(Buffer * const pBD, short mark);
int b_mode(Buffer * const pBD);
size_t b_incfactor(Buffer * const pBD);
int b_load(FILE * const fi, Buffer * const pBD);
int b_isempty(Buffer * const pBD);
int b_eob(Buffer * const pBD);
char b_getc(Buffer * const pBD);
int b_print(Buffer * const pBD);
Buffer * b_compact(Buffer * const pBD, char symbol);
char b_rflag(Buffer * const pBD);
short b_retract(Buffer * const pBD);
short b_reset(Buffer * const pBD);
short b_getcoffset(Buffer * const pBD);
int b_rewind(Buffer * const pBD);
char * b_location(Buffer * const pBD, short loc_offset);

#endif

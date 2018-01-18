/*******************************************************************************
File name:			buffer.c
Compiler:			Microsoft Visual Studio 2015
Author:				Hasan Skaiky, 040 ### ###
Mohamed Elmekki, 040 ### ###
Course:				CST 8152 – Compilers, Lab Section: 013
Assignment:			2
Date:				November 30, 2017
Professor:			Svillen Ranev
Purpose:			To implement a buffer that can operate in three different
modes. The buffer implementation is based on two associated
data structures: a buffer descriptor, and an array of
characters.
Function list:		b_allocate()
b_addc()
b_clear()
b_free()
b_isfull()
b_limit()
b_capacity()
b_mark()
b_mode()
b_incfactor()
b_load()
b_isempty()
b_eob()
b_getc()
b_print()
b_compact()
b_rflag()
b_retract()
b_reset()
b_getcoffset()
b_rewind()
b_location()
*******************************************************************************/

#include "buffer.h"

/*******************************************************************************
Purpose:			This function creates a new buffer in heap memory
Author:				Hasan Skaiky, Mohamed Elmekki
History/Versions:	1.0
Called functions:	free()
calloc()
malloc()
Parameters:			short init_capacity
The desired capacity for the buffer
Valid if between 1 and SHRT_MAX (limits.h library)
char inc_factor
The desired increment factor for the buffer
Valid between 1 and 255 in additive mode
Valid between 1 and 100 in multiplicative mode
char o_mode
The desired operational mode for the buffer
Valid if 'f', 'a', or 'm'
Return value:		Buffer *
A pointer to a completely allocated buffer structure
NULL
To indicate failure
Algorithm:			Check arguments for validity
Attempt to allocate memory for Buffer structure; validate
Attempt to allocate memory for dynamic char array; validate
Determine and set the buffer's mode and increment factor
*******************************************************************************/
Buffer * b_allocate(short init_capacity, char inc_factor, char o_mode) {
	/*A pointer to the buffer to be created in heap memory*/
	Buffer * buffer;

	if (init_capacity < ZERO_INIT_CAPACITY || init_capacity > MAX_INIT_CAPACITY) {
		return NULL;
	}

	/*If specified operation mode is fixed*/
	if (o_mode == 'f' || inc_factor == 0) {
		if (init_capacity == ZERO_INIT_CAPACITY) {
			return NULL;
		}

		/*Allocate memory for the buffer structure*/
		buffer = (Buffer *)calloc(1, sizeof(Buffer));

		if (!buffer) {
			return NULL;
		}

		/*Allocate memory for the buffer's string*/
		buffer->cb_head = (char *)malloc(sizeof(char) * init_capacity);

		if (!buffer->cb_head) {
			free(buffer);
			return NULL;
		}

		buffer->mode = FIXED_MODE;
		buffer->inc_factor = FIXED_INC_FACTOR;
	}
	/*If specified operation mode is additive*/
	else if (o_mode == 'a'
		&& inc_factor > 0
		&& inc_factor <= ADD_MODE_MAX_INC_FACTOR) {

		/*Allocate memory for the buffer structure*/
		buffer = (Buffer *)calloc(1, sizeof(Buffer));

		if (!buffer) {
			return NULL;
		}

		/*Allocate memory for the buffer's string*/
		buffer->cb_head = (char *)malloc(sizeof(char) * init_capacity);

		if (!buffer->cb_head) {
			free(buffer);
			return NULL;
		}

		buffer->mode = ADD_MODE;
		buffer->inc_factor = inc_factor;
	}
	/*If specified operation mode is multiplicative*/
	else if (o_mode == 'm'
		&& inc_factor > 0
		&& inc_factor <= MULTI_MODE_MAX_INC_FACTOR) {

		/*Allocate memory for the buffer structure*/
		buffer = (Buffer *)calloc(1, sizeof(Buffer));

		if (!buffer) {
			return NULL;
		}

		/*Allocate memory for the buffer's string*/
		buffer->cb_head = (char *)malloc(sizeof(char) * init_capacity);

		if (!buffer->cb_head) {
			free(buffer);
			return NULL;
		}

		buffer->mode = MULTI_MODE;
		buffer->inc_factor = inc_factor;
	}
	/*In case of fail condition*/
	else {
		return NULL;
	}

	/*Set the buffer's capacity*/
	buffer->capacity = init_capacity;

	return buffer;
}

/*******************************************************************************
Purpose:			This buffer attempts to add the given char argument to the
character buffer
Author:				Hasan Skaiky, Mohamed Elmekki
History/Versions:	1.0
Called functions:	realloc()
Parameters:			pBuffer const pBD
A pointer to the buffer to be operated on
Valid if not null
char symbol
The char to be added to the buffer
Return value:		Buffer *
A pointer to a buffer with the given character added
NULL
Indicates failure
Algorithm:
*******************************************************************************/
pBuffer b_addc(pBuffer const pBD, char symbol) {
	/*Pointer to new character buffer; used if character buffer is relocated*/
	char * new_char_buffer;
	/*Calculated new capacity of the buffer; used if buffer is resized*/
	short new_capacity = 0;
	/*The usable space available in the buffer*/
	short available_capacity = 0;
	/*The change in buffer capacity given multiplicative mode*/
	short new_increment = 0;

	if (!pBD) {
		return NULL;
	}

	pBD->r_flag = 0;

	/*If there is available space in the buffer*/
	if (pBD->addc_offset < pBD->capacity) {
		pBD->cb_head[pBD->addc_offset++] = symbol;
		return pBD;
	}

	/*If the buffer is at capacity*/
	if (pBD->addc_offset == pBD->capacity) {

		/*If the buffer is in fixed mode, end add procedure*/
		if (pBD->mode == FIXED_MODE) {
			return NULL;
		}

		/*If the buffer is in additive mode*/
		if (pBD->mode == ADD_MODE) {
			if (pBD->capacity == MAX_INIT_CAPACITY) {
				return NULL;
			}

			/*Calculate the new capacity relative to the buffer's mode*/
			new_capacity = (pBD->capacity + (unsigned char)pBD->inc_factor)
				* sizeof(char);

			if (new_capacity < 0) {
				return NULL;
			}

			if (new_capacity > MAX_INIT_CAPACITY) {
				new_capacity = MAX_INIT_CAPACITY;
			}
		}

		/*If the buffer is in multiplicative mode*/
		if (pBD->mode == MULTI_MODE) {
			if (pBD->capacity == MAX_INIT_CAPACITY) {
				return NULL;
			}

			/*Calculate the new capacity relative to the buffer's mode*/
			available_capacity = (MAX_INIT_CAPACITY)-pBD->capacity;
			new_increment = available_capacity
				* (unsigned char)pBD->inc_factor / 100;

			if (new_increment < 1) {
				new_increment = available_capacity;
			}

			new_capacity = (pBD->capacity + new_increment) * sizeof(char);

			if (new_capacity < 0 || new_capacity > MAX_INIT_CAPACITY) {
				new_capacity = MAX_INIT_CAPACITY;
			}
		}

		/*Reallocate new memory for the buffer's string*/
		new_char_buffer = (char *)realloc(pBD->cb_head, new_capacity);

		if (!new_char_buffer) {
			return NULL;
		}

		pBD->cb_head = new_char_buffer;

		/*Update the buffer's meta data on successful reallocation*/
		if (new_char_buffer != pBD->cb_head) {
			pBD->r_flag = SET_R_FLAG;
		}
		pBD->cb_head[pBD->addc_offset++] = symbol;
		pBD->capacity = new_capacity;

		return pBD;
	}

	return NULL;
}

/*******************************************************************************
Purpose:			Resets metadata regarding character array
Author:				Hasan Skaiky, Mohamed Elmekki
History/Versions:	1.0
Called functions:	N/A
Parameters:			Buffer * const pBD
A pointer to the buffer to be operated on
Valid if not null
Return value:		int
An integer indicator of success/failure
0 indicates success
-1 indicates failure
Algorithm:
*******************************************************************************/
int b_clear(Buffer * const pBD) {
	if (!pBD) {
		return RT_FAIL1;
	}

	pBD->addc_offset = 0;
	pBD->getc_offset = 0;
	pBD->markc_offset = 0;
	pBD->r_flag = 0;
	pBD->eob = 0;

	return 0;
}

/*******************************************************************************
Purpose:			To free all memory associated with the given buffer
Author:				Hasan Skaiky, Mohamed Elmekki
History/Versions:	1.0
Called functions:	free()
Parameters:			Buffer * const pBD
A pointer to the buffer to be operated on
Valid if not null
Return value:		N/A
Algorithm:
*******************************************************************************/
void b_free(Buffer * const pBD) {
	if (!pBD) {
		return;
	}

	if (pBD->cb_head) {
		free(pBD->cb_head);
	}

	free(pBD);

	return;
}

/*******************************************************************************
Purpose:			To determine if the given buffer is at capacity
Author:				Hasan Skaiky, Mohamed Elmekki
History/Versions:	1.0
Called functions:	N/A
Parameters:			Buffer * const pBD
A pointer to the buffer to be operated on
Valid if not null
Return value:		int
An integer indicator of the state of the buffer
-1 indicates failure
Algorithm:
*******************************************************************************/
int b_isfull(Buffer * const pBD) {
	if (!pBD) {
		return RT_FAIL1;
	}

	if (pBD->addc_offset == pBD->capacity) {
		return 1;
	}

	return 0;
}

/*******************************************************************************
Purpose:			Return the limit (size) of the given buffer
Author:				Hasan Skaiky, Mohamed Elmekki
History/Versions:	1.0
Called functions:	N/A
Parameters:			Buffer * const pBD
A pointer to the buffer to be operated on
Valid if not null
Return value:		short
The limit of the buffer
-1 indicates failure
Algorithm:
*******************************************************************************/
short b_limit(Buffer * const pBD) {
	if (!pBD) {
		return RT_FAIL1;
	}

	return pBD->addc_offset;
}

/*******************************************************************************
Purpose:			Return the capacity of the given buffer
Author:				Hasan Skaiky, Mohamed Elmekki
History/Versions:	1.0
Called functions:	N/A
Parameters:			Buffer * const pBD
A pointer to the buffer to be operated on
Valid if not null
Return value:		short
The capacity of the buffer
-1 indicates failure
Algorithm:
*******************************************************************************/
short b_capacity(Buffer * const pBD) {
	if (!pBD) {
		return RT_FAIL1;
	}

	return pBD->capacity;
}

/*******************************************************************************
Purpose:			Set the markc_offset field of the given buffer
Author:				Hasan Skaiky, Mohamed Elmekki
History/Versions:	1.0
Called functions:	N/A
Parameters:			Buffer * const pBD
A pointer to the buffer to be operated on
Valid if not null
short mark
The desired markc_offset value
Return value:		short
The desired markc_offset value
-1 indicates failure
Algorithm:
*******************************************************************************/
short b_mark(Buffer * const pBD, short mark) {
	if (!pBD) {
		return RT_FAIL1;
	}

	if (mark < 0 || mark > pBD->addc_offset) {
		return RT_FAIL1;
	}

	return pBD->markc_offset = mark;
}

/*******************************************************************************
Purpose:			Returns the value of the mode field of the given buffer
Author:				Hasan Skaiky, Mohamed Elmekki
History/Versions:	1.0
Called functions:	N/A
Parameters:			Buffer * const pBD
A pointer to the buffer to be operated on
Valid if not null
Return value:		int
The buffer's mode
-1 indicates failure
Algorithm:
*******************************************************************************/
int b_mode(Buffer * const pBD) {
	if (!pBD) {
		return RT_FAIL2;
	}

	return pBD->mode;
}

/*******************************************************************************
Purpose:			Return the increment factor of the given buffer
Author:				Hasan Skaiky, Mohamed Elmekki
History/Versions:	1.0
Called functions:	N/A
Parameters:			Buffer * const pBD
A pointer to the buffer to be operated on
Valid if not null
Return value:		size_t
Increment factor of buffer
INVALID_INC_FACTOR (256) indicates failure
Algorithm:
*******************************************************************************/
size_t b_incfactor(Buffer * const pBD) {
	if (!pBD) {
		return INVALID_INC_FACTOR;
	}

	return (unsigned char)pBD->inc_factor;
}

/*******************************************************************************
Purpose:			To read an open input file and writes to the buffer one
character at a time
Author:				Hasan Skaiky, Mohamed Elmekki
History/Versions:	1.0
Called functions:	fgetc()
feof()
b_addc()
Parameters:			FILE * const fi
A file pointer to the data source
Valid if not null
Buffer * const pBD
A pointer to the buffer to be operated on
Valid if not null
Return value:		int
The total number of characters loaded into the buffer
-1 indicates a runtime error
-2 indicates a failure to load character into buffer
Algorithm:			Loop calls of fgetc() until the end of the file is reached
*******************************************************************************/
int b_load(FILE * const fi, Buffer * const pBD) {
	char symbol;
	int count = 0;

	if (!pBD || !fi) {
		return RT_FAIL1;
	}

	while (1) {
		symbol = (char)fgetc(fi);

		if (feof(fi)) {
			break;
		}

		if (!b_addc(pBD, symbol)) {
			return LOAD_FAIL;
		}

		count++;
	}

	return count;
}

/*******************************************************************************
Purpose:			To determine if given buffer is empty
Author:				Hasan Skaiky, Mohamed Elmekki
History/Versions:	1.0
Called functions:	N/A
Parameters:			Buffer * const pBD
A pointer to the buffer to be operated on
Valid if not null
Return value:		int
1 indicates buffer is empty
0 indicates buffer is not empty
-1 indicates failure
Algorithm:
*******************************************************************************/
int b_isempty(Buffer * const pBD) {
	if (!pBD) {
		return RT_FAIL1;
	}

	if (pBD->addc_offset == 0) {
		return 1;
	}

	return 0;
}

/*******************************************************************************
Purpose:			Returns the eob field of the given buffer
Author:				Hasan Skaiky, Mohamed Elmekki
History/Versions:	1.0
Called functions:	N/A
Parameters:			Buffer * const pBD
A pointer to the buffer to be operated on
Valid if not null
Return value:		int
eob field of the given buffer
-1 indicates failure
Algorithm:
*******************************************************************************/
int b_eob(Buffer * const pBD) {
	if (!pBD) {
		return RT_FAIL1;
	}

	return pBD->eob;
}

/*******************************************************************************
Purpose:			To return the character located at getc_offset from the
buffer
Author:				Hasan Skaiky, Mohamed Elmekki
History/Versions:	1.0
Called functions:	N/A
Parameters:			Buffer * const pBD
A pointer to the buffer to be operated on
Valid if not null
Return value:		char
The buffer's addc_offset field
-1 indicates the buffer is full
-2 indicates failure
Algorithm:
*******************************************************************************/
char b_getc(Buffer * const pBD) {
	if (!pBD) {
		return RT_FAIL2;
	}

	if (pBD->getc_offset == pBD->addc_offset) {
		pBD->eob = 1;
		return RT_FAIL1;
	}

	pBD->eob = 0;
	return pBD->cb_head[pBD->getc_offset++];
}

/*******************************************************************************
Purpose:			To print, character by character, the contents of the buffer
to standard output
Author:				Hasan Skaiky, Mohamed Elmekki
History/Versions:	1.0
Called functions:	b_eob()
b_getc()
printf()
Parameters:			Buffer * const pBD
A pointer to the buffer to be operated on
Valid if not null
Return value:		int
The number of characters printed
-1 indicates failure
Algorithm:			Iterate through a valid character buffer
Print the values within it by calling the b_getc() function
Update all relevant meta data
*******************************************************************************/
int b_print(Buffer * const pBD) {
	int count = 0;
	char c;

	if (!pBD) {
		return RT_FAIL1;
	}

	if (pBD->addc_offset == 0) {
		printf("Empty buffer\n");
		return count;
	}

	while (1) {
		c = (char)b_getc(pBD);

		if (b_eob(pBD) == 1) {
			break;
		}

		printf("%c", c);
		count++;
	}

	printf("\n");

	return count;
}

/*******************************************************************************
Purpose:			Adds the given character to the buffer, and shrinks/expands
to trim unused buffer space
Author:				Hasan Skaiky, Mohamed Elmekki
History/Versions:	1.0
Called functions:	realloc()
Parameters:			Buffer * const pBD
A pointer to the buffer to be operated on
Valid if not null
Return value:		Buffer *
A pointer to a resized buffer with the given character
added
NULL
To indicate failure
Algorithm:			Reallocate given memory for the character array under
certain conditions
*******************************************************************************/
Buffer * b_compact(Buffer * const pBD, char symbol) {
	/*New calculated capacity of the buffer*/
	short new_capacity;
	/*The newly allocated buffer; used where buffer was reallocated*/
	char * new_char_buffer;

	if (!pBD) {
		return NULL;
	}

	pBD->r_flag = 0;

	new_capacity = ((pBD->addc_offset + 1) * sizeof(char));

	if (new_capacity < 0) {
		return NULL;
	}

	new_char_buffer = (char *)realloc(pBD->cb_head, new_capacity);

	if (!new_char_buffer) {
		return NULL;
	}

	pBD->cb_head = new_char_buffer;

	pBD->cb_head[pBD->addc_offset++] = symbol;
	pBD->capacity = new_capacity;
	if (pBD->cb_head == new_char_buffer) {
		pBD->r_flag = SET_R_FLAG;
	}

	return pBD;
}

/*******************************************************************************
Purpose:			Returns the value of the r_flag field of the given buffer
Author:				Hasan Skaiky, Mohamed Elmekki
History/Versions:	1.0
Called functions:	N/A
Parameters:			Buffer * const pBD
A pointer to the buffer to be operated on
Valid if not null
Return value:		char
The buffer's r_flag
-1 indicates failure
Algorithm:
*******************************************************************************/
char b_rflag(Buffer * const pBD) {
	if (!pBD) {
		return RT_FAIL1;
	}

	return pBD->r_flag;
}

/*******************************************************************************
Purpose:			To decrement the getc_offset field of the given buffer
Author:				Hasan Skaiky, Mohamed Elmekki
History/Versions:	1.0
Called functions:	N/A
Parameters:			Buffer * const pBD
A pointer to the buffer to be operated on
Valid if not null
Return value:		short
The buffer's updated getc_offset field
-1 indicates failure
Algorithm:
*******************************************************************************/
short b_retract(Buffer * const pBD) {
	if (!pBD) {
		return RT_FAIL1;
	}

	if (pBD->getc_offset == 0) {
		return RT_FAIL1;
	}

	return --pBD->getc_offset;
}

/*******************************************************************************
Purpose:			Reset getc_offset and markc_offset to prepare to read the
buffer again
Author:				Hasan Skaiky, Mohamed Elmekki
History/Versions:	1.0
Called functions:	N/A
Parameters:			Buffer * const pBD
A pointer to the buffer to be operated on
Valid if not null
Return value:		short
The buffer's getc_offset field
-1 indicates failure
Algorithm:
*******************************************************************************/
short b_reset(Buffer * const pBD) {
	if (!pBD) {
		return RT_FAIL1;
	}

	return pBD->getc_offset = pBD->markc_offset;
}

/*******************************************************************************
Purpose:			Returns the value of the getc_offset field of the given
buffer
Author:				Hasan Skaiky, Mohamed Elmekki
History/Versions:	1.0
Called functions:	N/A
Parameters:			Buffer * const pBD
A pointer to the buffer to be operated on
Valid if not null
Return value:		short
The buffer's getc_offset value
-1 indicates failure
Algorithm:
*******************************************************************************/
short b_getcoffset(Buffer * const pBD) {
	if (!pBD) {
		return RT_FAIL1;
	}

	return pBD->getc_offset;
}

/*******************************************************************************
Purpose:			This function resets the getc_offset and markc_offset of the
given buffer
Author:				Hasan Skaiky, Mohamed Elmekki
History/Versions:	1.0
Called functions:	N/A
Parameters:			Buffer * const pBD
A pointer to the buffer to be operated on
Valid if not null
Return value:		int
0: new value of buffer's getc_offset and addc_offset
-1 indicates failure
Algorithm:
*******************************************************************************/
int b_rewind(Buffer * const pBD) {
	if (!pBD) {
		return RT_FAIL1;
	}

	return pBD->getc_offset = pBD->markc_offset = 0;
}

/*******************************************************************************
Purpose:			To return the character in the buffer at the location
specified by the given parameter
Author:				Hasan Skaiky, Mohamed Elmekki
History/Versions:	1.0
Called functions:	N/A
Parameters:			Buffer * const pBD
A pointer to the buffer to be operated on
Valid if not null
short loc_offset
The location desired as an offset from the beginning
of the buffer
Return value:		char *
A pointer to the given character buffer
NULL
Indicates failure
Algorithm:
*******************************************************************************/
char * b_location(Buffer * const pBD, short loc_offset) {
	if (!pBD) {
		return NULL;
	}

	if (loc_offset >= pBD->addc_offset || loc_offset < 0) {
		return NULL;
	}

	return pBD->cb_head + loc_offset;
}
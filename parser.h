#pragma once
#include "buffer.h"
#include "token.h"

//#define DEBUG
//#define DEBUG_MATCH
//#define DEBUG_SYN_PR

#define NO_ATTR  -1
#define KWT_SIZE 10

typedef enum KeyWords {
	ELSE,
	FALSE,
	IF,
	PLATYPUS,
	READ,
	REPEAT,
	THEN,
	TRUE,
	WHILE,
	WRITE
};

extern Token malar_next_token(Buffer *);
extern char * kw_table[KWT_SIZE];
extern Buffer * str_LTBL;
extern int line;

static Token lookahead;
static Buffer * sc_buf;
int synerrno = 0;

void parser(Buffer * in_buf);
void match(int pr_token_code, int pr_token_attribute);

/* Parser production functions */
/* RDPP Grammar 3.1 */
void program(void);//
void statements(void);//
void statements_p(void);//
void opt_statements(void);//

						  /* RDPP Grammar 3.2 */
void statement(void);

/* RDPP Grammar 3.2.1 */
void assignment_statement(void);
void assignment_expression(void);

/* RDPP Grammar 3.2.2 */
void selection_statement(void);

/* RDPP Grammar 3.2.3 */
void iteration_statement(void);
void precondition(void);

/* RDPP Grammar 3.2.4 & 3.2.5 */
void input_statement(void);
void output_statement(void);
void output_list(void);
void opt_variable_list(void);
void variable_list(void);
void variable_list_p(void);
void variable_identifier(void);

/* RDPP Grammar 3.3.1 */
void arithmetic_expression(void);
void unary_arithmetic_expression(void);
void additive_arithmetic_expression(void);
void additive_arithmetic_expression_p(void);
void multiplicative_arithmetic_expression(void);
void multiplicative_arithmetic_expression_p(void);
void primary_arithmetic_expression(void);

/* RDPP Grammar 3.3.2 */
void string_expression(void);
void string_expression_p(void);
void primary_string_expression(void);

/* RDPP Grammar 3.3.3 */
void conditional_expression(void);

void logical_or_expression(void);
void logical_or_expression_p(void);
void logical_and_expression(void);
void logical_and_expression_p(void);

/* RDPP Grammar 3.3.4 */
void relational_expression(void);
void primary_a_relational_expression(void);
void primary_s_relational_expression(void);
void relational_operator(void);

void gen_incode(char * code);
void syn_eh(int);
void syn_printe(void);
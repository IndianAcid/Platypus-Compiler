#include "parser.h"

/*******************************************************************************
Authors:			Svillen Ranev
*******************************************************************************/
void parser(Buffer * in_buf) {
	sc_buf = in_buf;
	lookahead = malar_next_token(sc_buf);
	program();
	match(SEOF_T, NO_ATTR);
	gen_incode("Source file parsed");
}

/*******************************************************************************
Authors:			Hasan Skaiky, Mohamed Elmekki
*******************************************************************************/
void match(int pr_token_code, int pr_token_attribute) {

#ifdef DEBUG_MATCH
	printf("\t\tpr_token_code: %d pr_token_attribute: %d\n", pr_token_code, pr_token_attribute);
	printf("\t\tlookahead.code: %d lookahead.att: %d\n", lookahead.code, lookahead.attribute.get_int);
#endif

	/* If the required token code does not match the current one, engage panic
	* mode and return*/
	if (pr_token_code != lookahead.code) {
		syn_eh(pr_token_code);
		return;
	}

	switch (pr_token_code) {
		case KW_T:
		case LOG_OP_T:
		case ART_OP_T:
		case REL_OP_T:
			if (pr_token_attribute != lookahead.attribute.get_int) {
				syn_eh(pr_token_attribute);
				return;
			}

		default:
			break;
	}

	if (lookahead.code == SEOF_T) {
		return;
	}

	lookahead = malar_next_token(sc_buf);

	if (lookahead.code == ERR_T) {
		syn_printe();
		lookahead = malar_next_token(sc_buf);
		synerrno++;
	}
}

/*******************************************************************************
Authors:			Hasan Skaiky, Mohamed Elmekki
Grammar:			<program> -> KW_T(PLATYPUS) { opt_statements() }
First Set:			FIRST(<program>) = { KW_T(PLATYPUS) }
*******************************************************************************/
void program(void) {

#ifdef DEBUG
	printf("\tprogram called\n");
#endif

	match(KW_T, PLATYPUS); match(LBR_T, NO_ATTR); opt_statements();
	match(RBR_T, NO_ATTR);
	gen_incode("Program parsed");
}

/*******************************************************************************
Authors:			Hasan Skaiky, Mohamed Elmekki
Grammar:			<statements> -> <statement> <statements'>
First Set:			FIRST(<statements>) = { AVID_T, SVID_T, IF, WHILE, READ, WRITE }
*******************************************************************************/
void statements(void) {

#ifdef DEBUG
	printf("\tstatements called\n");
#endif

	statement();
	statements_p();
}

/*******************************************************************************
Authors:			Hasan Skaiky, Mohamed Elmekki
Grammar:			<statements'> -> <statement> <statements'>
First Set:			FIRST(<statements'>) = { AVID_T, SVID_T, IF, WHILE, READ, WRITE, e }
*******************************************************************************/
void statements_p(void) {

#ifdef DEBUG
	printf("\tstatements_p called\n");
#endif

	switch (lookahead.code) {
		case AVID_T:
		case SVID_T:
			statement();
			statements_p();
			return;
		case KW_T:
			switch (lookahead.attribute.kwt_idx) {
				case IF:
				case WHILE:
				case READ:
				case WRITE:
					statement();
					statements_p();
					return;
			}

		return;
	}
}

/*******************************************************************************
Authors:			Svillen Ranev
Grammar:			<opt_statements> -> <statements> | e
First Set:			FIRST(<opt_statements>) = { AVID_T, SVID_T, IF, WHILE, READ, WRITE, e }
*******************************************************************************/
void opt_statements(void) {

#ifdef DEBUG
	printf("\topt_statements called\n");
#endif

	switch (lookahead.code) {
	case AVID_T:
	case SVID_T:
		statements();
		return;
	case KW_T:
		switch (lookahead.attribute.get_int) {
			case IF:
			case WHILE:
			case READ:
			case WRITE:
				statements();
				return;
			default:
				break;
		}
		return;
	default:
		gen_incode("Opt_statements parsed");
		return;
	}
}

/*******************************************************************************
Authors:			Hasan Skaiky, Mohamed Elmekki
Grammar:			<statements> ->
						<assignment statement>
						| <selection statement>
						| <iteration statement>
						| <input statement>
						| <output statement>
First Set:			FIRST(<statement>) = { AVID_T, SVID_T, IF, WHILE, READ, WRITE }
*******************************************************************************/
void statement(void) {

#ifdef DEBUG
	printf("\tstatement called\n");
#endif

	switch (lookahead.code) {
		case AVID_T:
		case SVID_T:
			assignment_statement();
			return;
		case KW_T:
			switch (lookahead.attribute.kwt_idx) {
				case IF:
					selection_statement();
					return;
				case WHILE:
					iteration_statement();
					return;
				case READ:
					input_statement();
					return;
				case WRITE:
					output_statement();
					return;
				default:
					syn_printe();
					return;
			}

			return;
		default:
			syn_printe();
			return;
	}
}

/*******************************************************************************
Authors:			Hasan Skaiky, Mohamed Elmekki
Grammar:			<assignment_expression> -> <assignment_expression>;
First Set:			FIRST(<assigment expression>) = { AVID_T, SVID_T }
*******************************************************************************/
void assignment_statement(void) {

#ifdef DEBUG
	printf("\tassignment_statement called\n");
#endif

	assignment_expression();
	match(EOS_T, NO_ATTR);
	gen_incode("Assignment statement parsed");
}

/*******************************************************************************
Authors:			Hasan Skaiky, Mohamed Elmekki
Grammar:			<assignment expression> ->
						AVID = <arithmetic expression>
						| SVID = <string expression>
First Set:			FIRST(<assignment expression>) = { AVID_T, SVID_T }
*******************************************************************************/
void assignment_expression(void) {

#ifdef DEBUG
	printf("\tassignment_expression called\n");
#endif

	switch (lookahead.code) {
		case AVID_T:
			match(AVID_T, lookahead.attribute.get_int);
			match(ASS_OP_T, NO_ATTR);
			arithmetic_expression();
			gen_incode("Assignment expression (arithmetic) parsed");
			return;
		case SVID_T:
			match(SVID_T, lookahead.attribute.get_int);
			match(ASS_OP_T, NO_ATTR);
			string_expression();
			gen_incode("Assignment expression (string) parsed");
			return;
		default:
			syn_printe();
			return;
	}
}

/*******************************************************************************
Authors:			Hasan Skaiky, Mohamed Elmekki
Grammar:			<selection statement> ->
						IF TRUE (<conditional expression>) THEN
							{<opt_statements>}
						ELSE {
							<opt_statements>
						};
First Set:			FIRST(<selection statement>) = { IF }
*******************************************************************************/
void selection_statement(void) {

#ifdef DEBUG
	printf("\tselection_stament called\n");
#endif
	match(KW_T, IF);
	match(KW_T, TRUE);
	match(LPR_T, NO_ATTR);
	conditional_expression();
	match(RPR_T, NO_ATTR);
	match(KW_T, THEN);
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);
	match(KW_T, ELSE);
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("Selection statement parsed");
}

/*******************************************************************************
Authors:			Hasan Skaiky, Mohamed Elmekki
Grammar:			<iteration statement> ->
						WHILE <pre-condition> (<conditional expression>)
							REPEAT {<statements>};
First Set:			FIRST (<iteration statements>) = { WHILE }
*******************************************************************************/
void iteration_statement(void) {

#ifdef DEBUG
	printf("\titeration_statement called\n");
#endif

	match(KW_T, WHILE);
	precondition();
	match(LPR_T, NO_ATTR);
	conditional_expression();
	match(RPR_T, NO_ATTR);
	match(KW_T, REPEAT);
	match(LBR_T, NO_ATTR);
	statements();
	match(RBR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("Iteration statement parsed");
}

/*******************************************************************************
Authors:			Hasan Skaiky, Mohamed Elmekki
Grammar:			<pre-condition> -> TRUE | FALSE
First Set:			FIRST(<pre-condition>) = { TRUE, FALSE }
*******************************************************************************/
void precondition(void) {
	switch (lookahead.code) {
		case KW_T:
			switch (lookahead.attribute.kwt_idx) {
				case TRUE:
				case FALSE:
					match(lookahead.code, lookahead.attribute.kwt_idx);
					return;
				default:
					syn_printe();
					return;
			}

			return;
		default:
			syn_printe();
			return;
	}
}

/*******************************************************************************
Authors:			Hasan Skaiky, Mohamed Elmekki
Grammar:			<input statement> -> READ (<variable list>);
First Set:			FIRST(<input statement>) = { READ }
*******************************************************************************/
void input_statement(void) {

#ifdef DEBUG
	printf("\tinput_statement called\n");
#endif

	match(KW_T, READ);
	match(LPR_T, NO_ATTR);
	variable_list();
	match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("Input statement parsed");
}

/*******************************************************************************
Authors:			Hasan Skaiky, Mohamed Elmekki
Grammar:			<output statement> -> WRITE(<output list>);
First Set:			FIRST(<output statement>) = { WRITE }
*******************************************************************************/
void output_statement(void) {

#ifdef DEBUG
	printf("\toutput_statement called\n");
#endif

	match(KW_T, WRITE);
	match(LPR_T, NO_ATTR);
	output_list();
	match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("Output statement parsed");
}

/*******************************************************************************
Authors:			Hasan Skaiky, Mohamed Elmekki
Grammar:			<output list> -> <variable list> | e
First Set:			FIRST(<output list>) = { AVID_T, SVID_T, STR_T, e }
*******************************************************************************/
void output_list(void) {

#ifdef DEBUG
	printf("\toutput_list called\n");
#endif

	switch (lookahead.code) {
		case AVID_T:
		case SVID_T:
			opt_variable_list();
			return;
		case STR_T:
			match(STR_T, NO_ATTR);
			gen_incode("Output list (string literal) parsed");
			return;
		default:
			gen_incode("Output list (empty) parsed");
			return;
	}
}

/*******************************************************************************
Authors:			Hasan Skaiky, Mohamed Elmekki
Grammar:			<opt variable list> -> <variable list> | e
First Set:			FIRST(<opt variable list>) = { AVID_T, SVID_T, e }
*******************************************************************************/
void opt_variable_list(void) {
	switch (lookahead.code) {
		case AVID_T:
		case SVID_T:
			variable_list();
			return;
		default:
			return;
	}
}

/*******************************************************************************
Authors:			Hasan Skaiky, Mohamed Elmekki
Grammar:			<variable list> -> <variable identifier> <variable list'>
First Set:			FIRST(<variable list>) = { AVID_T, SVID_T }
*******************************************************************************/
void variable_list(void) {

#ifdef DEBUG
	printf("\tvariable_list called\n");
#endif

	variable_identifier();
	variable_list_p();
	gen_incode("Variable list parsed");
	return;
}

/*******************************************************************************
Authors:			Hasan Skaiky, Mohamed Elmekki
Grammar:			<variable list'> -> , <variable identifier> <variable list'> | e
First Set:			FIRST(<variable list'>) = { , , e }
*******************************************************************************/
void variable_list_p(void) {

#ifdef DEBUG
	printf("\tvariable_list_p called\n");
#endif

	switch (lookahead.code) {
		case COM_T:
			match(COM_T, NO_ATTR);
			variable_identifier();
			variable_list_p();
			return;
	}
}

/*******************************************************************************
Authors:			Hasan Skaiky, Mohamed Elmekki
Grammar:			<variable identifier> -> AVID_T, SVID_T
First Set:			FIRST(<variable identifier> = { AVID_T, SVID_T }
*******************************************************************************/
void variable_identifier(void) {

#ifdef DEBUG
	printf("\tvariable identifier called\n");
#endif

	switch (lookahead.code) {
		case AVID_T:
		case SVID_T:
			match(lookahead.code, NO_ATTR);
			return;
		default:
			syn_printe();
			return;
	}
}

/*******************************************************************************
Authors:			Hasan Skaiky, Mohamed Elmekki
Grammar:			<arithmetic expression> ->
						<unary arithmetic expression>
						| <additive arithmetic expression>
First Set:			FIRST(<arithmetic expression>) = { -, +, AVID_T, FPL_T, INL_T, ( }
*******************************************************************************/
void arithmetic_expression(void) {

#ifdef DEBUG
	printf("\tarithmetic_expression called\n");
#endif

	switch (lookahead.code) {
		case ART_OP_T:
			switch (lookahead.attribute.arr_op) {
				case PLUS:
				case MINUS:
					unary_arithmetic_expression();
					gen_incode("Unary arithmetic expression parsed");
					gen_incode("Arithmetic expression parsed");
					return;
				default:
					syn_printe();
					return;
			}

			return;
		case AVID_T:
		case FPL_T:
		case INL_T:
		case LPR_T:
			additive_arithmetic_expression();
			gen_incode("Arithmetic expression parsed");
			return;
		default:
			syn_printe();
			gen_incode("Arithmetic expression parsed");
			return;
	}
}

/*******************************************************************************
Authors:			Hasan Skaiky, Mohamed Elmekki
Grammar:			<unary arithmetic expression> ->
						- <primary arithmetic expression>
						| + <primary arithmetic expression>
First Set:			FIRST(<unary arithmetic expression>) = { -, + }
*******************************************************************************/
void unary_arithmetic_expression(void) {

#ifdef DEBUG
	printf("\tunary_arithmetic_expression called\n");
#endif

	switch (lookahead.code) {
		case ART_OP_T:
			switch (lookahead.attribute.arr_op) {
				case MINUS:
				case PLUS:
					match(lookahead.code, lookahead.attribute.arr_op);
					primary_arithmetic_expression();
					return;
				default:
					syn_printe();
					return;
			}

			return;
		default:
			syn_printe();
			return;
	}
}

/*******************************************************************************
Authors:			Hasan Skaiky, Mohamed Elmekki
Grammar:			<multiplicative arithmetic expression> <additive arithmetic expression'>
First Set:			FIRST(<additive arithmetic expression>) = { AVID_T, FPL_T, INL_T, ( }
*******************************************************************************/
void additive_arithmetic_expression(void) {

#ifdef DEBUG
	printf("\tadditive_arithmetic_expression called\n");
#endif

	multiplicative_arithmetic_expression();
	additive_arithmetic_expression_p();
}

/*******************************************************************************
Authors:			Hasan Skaiky, Mohamed Elmekki
Grammar:			<additive arithmetic expression'> ->
						+ <multiplicitave arithmetic expression> <additive arithmetic expression'>
						| - <multiplicitave arithmetic expression> <additive arithmetic expression'>
						| e
First Set:			FIRST(<additive arithmetic expression'>) = { +, -, e }
*******************************************************************************/
void additive_arithmetic_expression_p(void) {

#ifdef DEBUG
	printf("\tadditive_arithmetic_expression_p called\n");
#endif

	switch (lookahead.code) {
		case ART_OP_T:
			switch (lookahead.attribute.arr_op) {
				case PLUS:
				case MINUS:
					match(lookahead.code, lookahead.attribute.arr_op);
					multiplicative_arithmetic_expression();
					additive_arithmetic_expression_p();
					gen_incode("Additive arithmetic expression parsed");
					return;
				default:
					return;
			}

			return;
		default:
			return;
	}
}

/*******************************************************************************
Authors:			Hasan Skaiky, Mohamed Elmekki
Grammar:			<multiplicative arithmetic expression> ->
						<primary arithmetic expression> <multipliciative arithmetic expression'>
First Set:			FIRST(<multiplicative arithmetic expression>) = { AVID_T, FPL_T, INL_T, ( }
*******************************************************************************/
void multiplicative_arithmetic_expression(void) {

#ifdef DEBUG
	printf("\tmultiplicative_arithmetic_expression called\n");
#endif

	primary_arithmetic_expression();
	multiplicative_arithmetic_expression_p();
}

/*******************************************************************************
Authors:			Hasan Skaiky, Mohamed Elmekki
Grammar:			<multiplicative arithmetic expression'> ->
						+ <primary arithmetic expression> <multiplicative arithmetic expression'>
						| / <primary arithmetic expression> <multiplicative arithmetic expression'>
						| e
First Set:			FIRST(<multiplicative arithmetic expression'>) = { *, /, e }
*******************************************************************************/
void multiplicative_arithmetic_expression_p(void) {

#ifdef DEBUG
	printf("\tmultiplicative_arithmetic_expression_p called\n");
#endif

	switch (lookahead.code) {
		case ART_OP_T:
			switch (lookahead.attribute.arr_op) {
				case MULT:
				case DIV:
					match(ART_OP_T, lookahead.attribute.arr_op);
					primary_arithmetic_expression();
					multiplicative_arithmetic_expression_p();
					gen_incode("Multiplicative arithmetic expression parsed");
					return;
				default:
					return;
			}

			return;
		default:
			return;
	}
}

/*******************************************************************************
Authors:			Hasan Skaiky, Mohamed Elmekki
Grammar:			<primary arithmetic expression> -> AVID_T | FPL_T
First Set:			FIRST(<primary arithmetic expression>) = { AVID_T, FPL_T, INL_T, ( }
*******************************************************************************/
void primary_arithmetic_expression(void) {

#ifdef DEBUG
	printf("\tprimary_arithmetic_expression called\n");
#endif

	switch (lookahead.code) {
	case AVID_T:
	case FPL_T:
	case INL_T:
		match(lookahead.code, NO_ATTR);
		break;
	case LPR_T:
		match(LPR_T, NO_ATTR);
		arithmetic_expression();
		match(RPR_T, NO_ATTR);
		break;
	default:
		syn_printe();
	}

	gen_incode("Primary arithmetic expression parsed");
}

/*******************************************************************************
Authors:			Hasan Skaiky, Mohamed Elmekki
Grammar:			<string expression> -> <string expression> # <primary string expression>
					| <primary string expression>
First Set:			FIRST(<string expression>) = { SVID_T, STR_T }
*******************************************************************************/
void string_expression(void) {

#ifdef DEBUG
	printf("\tstring_expression called\n");
#endif

	primary_string_expression();
	string_expression_p();
	gen_incode("String expression parsed");
}

/*******************************************************************************
Authors:			Hasan Skaiky, Mohamed Elmekki
Grammar:			<string expression'> ->
						# <primary string expression> <string expression'>
						| e
First Set:			FIRST(<string expression'>) = { #, e }
*******************************************************************************/
void string_expression_p(void) {

#ifdef DEBUG
	printf("\tstring_expression called\n");
#endif

	if (lookahead.code != SCC_OP_T) {
		return;
	}

	match(SCC_OP_T, NO_ATTR);
	primary_string_expression();
	string_expression_p();
}

/*******************************************************************************
Authors:			Hasan Skaiky, Mohamed Elmekki
Grammar:			<primary string expression> -> SVID_T | STR_T
First Set:			FIRST(<primary string expression>) = { SVID_T, STR_T }
*******************************************************************************/
void primary_string_expression(void) {

#ifdef DEBUG
	printf("\tprimary_string_expression called\n");
#endif

	switch (lookahead.code) {
		case SVID_T:
		case STR_T:
			match(lookahead.code, NO_ATTR);
			gen_incode("Primary string expression parsed");
			break;
		default:
			syn_printe();
			return;
	}
}

/*******************************************************************************
Authors:			Hasan Skaiky, Mohamed Elmekki
Grammar:			<conditional expression> -> <logical OR expression>
First Set:			FIRST(<conditional expression>) = { AVID_T, FPL_T, INLT, SVID_T, STR_T }
*******************************************************************************/
void conditional_expression(void) {

#ifdef DEBUG
	printf("\tconditional_expression called\n");
#endif

	logical_or_expression();
	gen_incode("Conditional expression parsed");
}

/*******************************************************************************
Authors:			Hasan Skaiky, Mohamed Elmekki
Grammar:			<logical OR expression> ->
						<logical AND expression> <logical OR expression'>
First Set:			FIRST(<logical OR expression>) = { AVID_T, FPL_T, INL_T, SCID_T, STR_T }
*******************************************************************************/
void logical_or_expression(void) {

#ifdef DEBUG
	printf("\tlogical_or_expression called\n");
#endif

	logical_and_expression();
	logical_or_expression_p();
}

/*******************************************************************************
Authors:			Hasan Skaiky, Mohamed Elmekki
Grammar:			<logical or expression'> ->
						.OR. <logical ANDexpression> <logical OR expression'>
						| e
First Set:			FIRST(<logical or expression'>) = { .OR., e }
*******************************************************************************/
void logical_or_expression_p(void) {

#ifdef DEBUG
	printf("\tlogical_or_expression_p called\n");
#endif

	switch (lookahead.code) {
		case LOG_OP_T:
			switch (lookahead.attribute.log_op) {
				case OR:
					match(LOG_OP_T, OR);
					logical_and_expression();
					logical_or_expression_p();
					gen_incode("Logical OR expression parsed");
					return;
				default:
					return;
			}

			return;
		default:
			return;
	}
}

/*******************************************************************************
Authors:			Hasan Skaiky, Mohamed Elmekki
Grammar:			<logical AND expression> ->
						<relational expression> <logical AND expression'>
First Set:			FIRST(<logical AND expression>) = { AVID_T, FPL_T, INL_T, SVID_T, STR_T }
*******************************************************************************/
void logical_and_expression(void) {

#ifdef DEBUG
	printf("\tlogical_and_expression called\n");
#endif

	relational_expression();
	logical_and_expression_p();
}

/*******************************************************************************
Authors:			Hasan Skaiky, Mohamed Elmekki
Grammar:			<logical and expression'> ->
						.AND. <relational expression> <logical AND expression'>
						| e
First Set:			FIRST(<logical and expression'>) = { .AND., e }
*******************************************************************************/
void logical_and_expression_p(void) {

#ifdef DEBUG
	printf("\tlogical_and_expression_p called\n");
#endif

	switch (lookahead.code) {
		case LOG_OP_T:
			switch (lookahead.attribute.log_op) {
				case AND:
					match(LOG_OP_T, AND);
					relational_expression();
					logical_and_expression_p();
					gen_incode("Logical AND expression parsed");
				default:
					return;
			}

			return;
		default:
			return;
	}
}

/*******************************************************************************
Authors:			Hasan Skaiky, Mohamed Elmekki
Grammar:			<relational expression> ->
						<primary a_relational expression> <relational operator> <primary a_relational expression>
						| <primary s_relational expression> <relational operator> <primary s_relational expression>
						
First Set:			First(<relational expression>) = { ==, <>, >, < }
*******************************************************************************/
void relational_expression(void) {

#ifdef DEBUG
	printf("\trelational_expression called\n");
#endif

	switch (lookahead.code) {
		case AVID_T:
		case FPL_T:
		case INL_T:
			primary_a_relational_expression();
			relational_operator();
			primary_a_relational_expression();
			break;
		case SVID_T:
		case STR_T:
			primary_s_relational_expression();
			relational_operator();
			primary_s_relational_expression();
			break;
		default:
			syn_printe();
			break;
	}

	gen_incode("Relational expression parsed");
}

/*******************************************************************************
Authors:			Hasan Skaiky, Mohamed Elmekki
Grammar:			<primary a_relational expression> ->
						AVIDT | FPL_T | INL_T
First Set:			FIRST(<primary a_relational expression>) = { AVID_T, FPL_T, INL_T }
*******************************************************************************/
void primary_a_relational_expression(void) {

#ifdef DEBUG
	printf("\tprimary_a_relational_expression called\n");
#endif

	switch (lookahead.code) {
		case AVID_T:
		case FPL_T:
		case INL_T:
			match(lookahead.code, NO_ATTR);
			break;
		default:
			syn_printe();
			break;
	}

	gen_incode("Primary a_relational expression parsed");
}

/*******************************************************************************
Authors:			Hasan Skaiky, Mohamed Elmekki
Grammar:			<primary s_relational expression> ->
						<primary string expression>
First Set:			FIRST(<primary s_relational expression>) = { SVID_T, STR_T }
*******************************************************************************/
void primary_s_relational_expression(void) {

#ifdef DEBUG
	printf("\tprimary_s_relational_expression called\n");
#endif

	switch (lookahead.code) {
		case STR_T:
		case SVID_T:
			primary_string_expression();
			break;
		default:
			syn_printe();
			break;
	}

	gen_incode("Primary s_relational expression parsed");
}

/*******************************************************************************
Authors:			Hasan Skaiky, Mohamed Elmekki
Grammar:			<relational operator> ->
						== | <> | > | <
First Set:			FIRST(<relational operator>) = { ==, <>, >, < }
*******************************************************************************/
void relational_operator(void) {

#ifdef DEBUG
	printf("\trelational_operator called\n");
#endif

	if (lookahead.code == REL_OP_T) {
		switch (lookahead.attribute.rel_op) {
			case EQ:
				match(REL_OP_T, EQ);
				return;
			case NE:
				match(REL_OP_T, NE);
				return;
			case GT:
				match(REL_OP_T, GT);
				return;
			case LT:
				match(REL_OP_T, LT);
				return;
			default:
				syn_printe();
				return;
		}
	}

	syn_printe();
}

/*******************************************************************************
Authors:			Hasan Skaiky, Mohamed Elmekki
*******************************************************************************/
void gen_incode(char * code) {
	printf("PLATY: %s\n", code);
}

/*******************************************************************************
Authors:			Hasan Skaiky, Mohamed Elmekki

*******************************************************************************/
void syn_eh(int sync_token_code) {
	syn_printe();
	synerrno++;

	while (lookahead.code != sync_token_code) {
		lookahead = malar_next_token(sc_buf);

		if (lookahead.code == sync_token_code) {
			if (lookahead.code != SEOF_T) {
				lookahead = malar_next_token(sc_buf);
			}

			return;
		}

		if (lookahead.code == SEOF_T) {
			exit(synerrno);
			return;
		}
	}
}

/*******************************************************************************
Authors:			Svillen Ranev
*******************************************************************************/
void syn_printe() {
	Token t = lookahead;

#ifdef DEBUG_SYN_PR
	printf("\t\t----TOKEN ERROR AHEAD:\n");
	printf("\t\tlookahead.code: %d lookahead.att: %d\n", lookahead.code, lookahead.attribute.get_int);
#endif

	printf("PLATY: Syntax error:  Line:%3d\n", line);
	printf("*****  Token code:%3d Attribute: ", t.code);
	switch (t.code) {
	case  ERR_T: /* ERR_T     0   Error token */
		printf("%s\n", t.attribute.err_lex);
		break;
	case  SEOF_T: /*SEOF_T    1   Source end-of-file token */\
		printf("NA\n");
		break;
	case  AVID_T: /* AVID_T    2   Arithmetic Variable identifier token */
	case  SVID_T:/* SVID_T    3  String Variable identifier token */
		printf("%s\n", t.attribute.vid_lex);
		break;
	case  FPL_T: /* FPL_T     4  Floating point literal token */
		printf("%5.1f\n", t.attribute.flt_value);
		break;
	case INL_T: /* INL_T      5   Integer literal token */
		printf("%d\n", t.attribute.get_int);
		break;
	case STR_T:/* STR_T     6   String literal token */
		printf("%s\n", b_location(str_LTBL, t.attribute.str_offset));
		break;

	case SCC_OP_T: /* 7   String concatenation operator token */
		printf("NA\n");
		break;

	case  ASS_OP_T:/* ASS_OP_T  8   Assignment operator token */
		printf("NA\n");
		break;
	case  ART_OP_T:/* ART_OP_T  9   Arithmetic operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  REL_OP_T: /*REL_OP_T  10   Relational operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  LOG_OP_T:/*LOG_OP_T 11  Logical operator token */
		printf("%d\n", t.attribute.get_int);
		break;

	case  LPR_T: /*LPR_T    12  Left parenthesis token */
		printf("NA\n");
		break;
	case  RPR_T: /*RPR_T    13  Right parenthesis token */
		printf("NA\n");
		break;
	case LBR_T: /*    14   Left brace token */
		printf("NA\n");
		break;
	case RBR_T: /*    15  Right brace token */
		printf("NA\n");
		break;

	case KW_T: /*     16   Keyword token */
		printf("%s\n", kw_table[t.attribute.get_int]);
		break;

	case COM_T: /* 17   Comma token */
		printf("NA\n");
		break;
	case EOS_T: /*    18  End of statement *(semi - colon) */
		printf("NA\n");
		break;
	default:
		printf("PLATY: Scanner error: invalid token code: %d\n", t.code);
	}/*end switch*/
}/* end syn_printe()*/
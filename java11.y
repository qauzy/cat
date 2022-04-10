%{
/*------------------------------------------------------------------
 * Copyright (C) 1996, 1997 Dmitri Bronnikov, All rights reserved.
 *
 * THIS GRAMMAR IS PROVIDED "AS IS" WITHOUT  ANY  EXPRESS  OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES  OF  MERCHANTABILITY  AND  FITNESS  FOR  A  PARTICULAR
 * PURPOSE, OR NON-INFRINGMENT.
 *
 * Bronikov@inreach.com
 *
 *------------------------------------------------------------------
 *
 * VERSION 1.03 DATE 11 NOV 1997
 *
 *------------------------------------------------------------------
 *
 * UPDATES
 *
 * 1.03 Added Java 1.1 changes:
 *      inner classes,
 *      anonymous classes,
 *      non-static initializer blocks,
 *      array initialization by new operator
 * 1.02 Corrected cast expression syntax
 * 1.01 All shift/reduce conflicts, except dangling else, resolved
 *
 *------------------------------------------------------------------
 *
 * PARSING CONFLICTS RESOLVED
 *
 * Some Shift/Reduce conflicts have been resolved at the expense of
 * the grammar defines a superset of the language. The following
 * actions have to be performed to complete program syntax checking:
 *
 * 1) Check that modifiers applied to a class, interface, field,
 *    or constructor are allowed in respectively a class, inteface,
 *    field or constructor declaration. For example, a class
 *    declaration should not allow other modifiers than abstract,
 *    final and public.
 *
 * 2) For an expression statement, check it is either increment, or
 *    decrement, or assignment expression.
 *
 * 3) Check that type expression in a cast operator indicates a type.
 *    Some of the compilers that I have tested will allow simultaneous
 *    use of identically named type and variable in the same scope
 *    depending on context.
 *
 * 4) Change lexical definition to change '[' optionally followed by
 *    any number of white-space characters immediately followed by ']'
 *    to OP_DIM token. I defined this token as [\[]{white_space}*[\]]
 *    in the lexer.
 *
 *------------------------------------------------------------------
 *
 * UNRESOLVED SHIFT/REDUCE CONFLICTS
 *
 * Dangling else in if-then-else
 *
 *------------------------------------------------------------------
 */
package parser


%}

%token ABSTRACT
%token BOOLEAN BREAK BYTE BYVALUE
%token CASE CAST CATCH CHAR CLASS CONST CONTINUE
%token DEFAULT DO DOUBLE
%token ELSE EXTENDS
%token FINAL FINALLY FLOAT FOR FUTURE
%token GENERIC GOTO
%token IF IMPLEMENTS IMPORT INNER INSTANCEOF INT INTERFACE
%token LONG
%token NATIVE NEW JNULL
%token OPERATOR OUTER
%token PACKAGE PRIVATE PROTECTED PUBLIC
%token REST RETURN
%token SHORT STATIC SUPER SWITCH SYNCHRONIZED
%token THIS THROW THROWS TRANSIENT TRY
%token VAR VOID VOLATILE
%token WHILE
%token OP_INC OP_DEC
%token OP_SHL OP_SHR OP_SHRR
%token OP_GE OP_LE OP_EQ OP_NE
%token OP_LAND OP_LOR
%token OP_DIM
%token ASS_MUL ASS_DIV ASS_MOD ASS_ADD ASS_SUB
%token ASS_SHL ASS_SHR ASS_SHRR ASS_AND ASS_XOR ASS_OR
%token IDENTIFIER LITERAL BOOLLIT

%token UNSIGNED_CHAR UNSIGNED_INT TYPEDEF_CHAR TYPEDEF_INT UNSIGNED TYPEDEF

%start CompilationUnit

%%

TypeSpecifier
	: TypeName
	| TypeName Dims
	{
		$$ = clips_tail_to_head($1,$2);
	}
	;

TypeName
	: PrimitiveType
	| QualifiedName
	;

ClassNameList
        : QualifiedName
        | ClassNameList ',' QualifiedName
        {
        	CLIPS *c = createFromString(",");
        	$$ = clips_tail_to_head(clips_tail_to_head($1,c),$3);
        }
	;

PrimitiveType
	: BOOLEAN
	  {
		$$ = ast.("bool",BOOLEAN);
	  }
	| CHAR
	{
		$$ = createFromStringWithToken("char",CHAR);
	}
	| BYTE
	{
		$$ = createFromStringWithToken("byte",BYTE);
	}
	| SHORT
	{
		$$ = createFromStringWithToken("short",SHORT);
	}
	| INT
	{
		$$ = createFromStringWithToken("int",INT);
	}
	| LONG
	{
		$$ = createFromStringWithToken("long",LONG);
	}
	| FLOAT
	{
		$$ = createFromStringWithToken("float",FLOAT);
	}
	| DOUBLE
	{
		$$ = createFromStringWithToken("double",DOUBLE);
	}
	| VOID
	{
		$$ = createFromStringWithToken("void",VOID);
	}
	;

CompilationUnit
	: ProgramFile
	{
		code_generator_cpp($1);
	}
    ;

ProgramFile
	: PackageStatement ImportStatements TypeDeclarations
	  {
		$$ =  clips_tail_to_head(clips_tail_to_head($1,$2),$3);
	  }
	| PackageStatement ImportStatements
	{
		$$ = clips_tail_to_head($1,$2);
	}
	| PackageStatement                  TypeDeclarations
	{
		$$ = clips_tail_to_head($1,$2);
	}
	|                  ImportStatements TypeDeclarations
	{
		$$ = clips_tail_to_head($1,$2);
	}
	| PackageStatement
	|                  ImportStatements
	|                                   TypeDeclarations
	;

PackageStatement
	: PACKAGE QualifiedName ';'
	  {
		$$ = handlePackageStatement($2);
	  }
	;

TypeDeclarations
	: TypeDeclaration
	| TypeDeclarations TypeDeclaration
	{
		$$ =  clips_tail_to_head($1,$2);
	}
	;

ImportStatements
	: ImportStatement
	  {
		$$ = handleImportStatement($1,0);
	  }
	| ImportStatements ImportStatement
	{
	$$ = handleImportStatement($1,$2);
	}
	;

ImportStatement
	: IMPORT QualifiedName ';'
	  {
		CLIPS *c1 = createFromStringWithToken("import",IMPORT);
		CLIPS *c2 = createFromString(";");
		$$ = clips_tail_to_head(clips_tail_to_head(c1,$2),c2);
	  }
	| IMPORT QualifiedName '.' '*' ';'
	{
		CLIPS *c1 = createFromStringWithToken("import",IMPORT);
		CLIPS *c2 = createFromString(".*");
		CLIPS *c3 = createFromString(";");
		$$ = clips_tail_to_head(clips_tail_to_head(clips_tail_to_head(c1,$2),c2),c3);
	}
	;

QualifiedName
	: IDENTIFIER
	  {
		$$ = auditIdentifier($1);
	  }
	| QualifiedName '.' IDENTIFIER
	{
		$$ = auditForArrow($1,$3);
	}
	
	;

TypeDeclaration
	: ClassHeader '{' FieldDeclarations '}'
	  {
		CLIPS *left = createFromString("{");
		CLIPS *right = createFromString("};");
		//$$ = clips_tail_to_head(clips_tail_to_head(clips_tail_to_head($1,left),$3),right);
		$$ =clips_tail_to_head(clips_tail_to_head(clips_tail_to_head($1,left),right),$3);
	  }
	| ClassHeader '{' '}'
	{
		CLIPS *left = createFromString("{");
		CLIPS *right = createFromString("};");
		$$ = clips_tail_to_head(clips_tail_to_head($1,left),right);
	}
	;

ClassHeader
	: Modifiers ClassWord IDENTIFIER Extends Interfaces
	  {
		CLIPS *c = $3;
		c->token = $2.token;
		CLIPS *c2 = createFromString(",");
		CLIPS *c3 = createFromString(":");
		$$ =  clips_tail_to_head(clips_tail_to_head(clips_tail_to_head(clips_tail_to_head(c,c3),$4),c2),$5);
		de_clips_list($1);
		maskClips($$,MASK_CLASSDEF);
	  }
	| Modifiers ClassWord IDENTIFIER Extends
	{
		CLIPS *c = $3;
		c->token = $2.token;
		CLIPS *c2 = createFromString(":");
		$$ = clips_tail_to_head(clips_tail_to_head(c,c2),$4);
		de_clips_list($1);
		maskClips($$,MASK_CLASSDEF);
		//
		//$$ = clips_tail_to_head(clips_tail_to_head($1,c),$4);
	}
	| Modifiers ClassWord IDENTIFIER       Interfaces
	{	
		CLIPS *c = $3;	
		c->token = $2.token;
		CLIPS *c2 = createFromString(":");
		$$ = clips_tail_to_head(clips_tail_to_head(c,c2),$4);
		de_clips_list($1);
		maskClips($$,MASK_CLASSDEF);
	}
	|           ClassWord IDENTIFIER Extends Interfaces
	{
		CLIPS *c = $2;
		c->token = $1.token;
		CLIPS *c2 = createFromString(",");
		CLIPS *c3 = createFromString(":");
		$$ =  clips_tail_to_head(clips_tail_to_head(clips_tail_to_head(clips_tail_to_head(c,c3),$3),c2),$4);
		maskClips($$,MASK_CLASSDEF);
	}
	| Modifiers ClassWord IDENTIFIER
	{
		CLIPS *c = $3;
		c->token = $2.token;
		//$$ = clips_tail_to_head($1,c);
		$$ = c;
		de_clips_list($1);
		maskClips($$,MASK_CLASSDEF);
	}
	|           ClassWord IDENTIFIER Extends
	{
		CLIPS *c = $2;
		c->token = $1.token;
		CLIPS *c2 = createFromString(":");
		$$ = clips_tail_to_head(clips_tail_to_head(c,c2),$3);
		maskClips($$,MASK_CLASSDEF);
	}
	|           ClassWord IDENTIFIER       Interfaces
	{
		CLIPS *c = $2;
		c->token = $1.token;
		CLIPS *c2 = createFromString(":");
		$$ = clips_tail_to_head(clips_tail_to_head(c,c2),$3);
		maskClips($$,MASK_CLASSDEF);
	}
	|           ClassWord IDENTIFIER
	{
		$$ = $2;
		$$->token = $1.token;
		maskClips($$,MASK_CLASSDEF);
	}
	;

Modifiers
	: Modifier
	| Modifiers Modifier
	{
		$$ = clips_tail_to_head($1,$2);
	}
	;

Modifier
	: ABSTRACT
	  {
	  		$$ = NewModifier($1.token);
	  }
	| FINAL
	{
			$$ = NewModifier($1.token);
		}
	| PUBLIC
	{
		$$ = NewModifier($1.token);
	}
	| PROTECTED
	{
			$$ = NewModifier($1.token);
	}
	| PRIVATE
	{
			$$ = NewModifier($1.token);
	}
	| STATIC
	{
			$$ = NewModifier($1.token);
	}
	| TRANSIENT
	{
			$$ = NewModifier($1.token);
	}
	| VOLATILE
	{
			$$ = NewModifier($1.token);
	}
	| NATIVE
	{
			$$ = NewModifier($1.token);
	}
	| SYNCHRONIZED
	{
			$$ = NewModifier($1.token);
	}
	;

ClassWord
	: CLASS
	| INTERFACE
	;

Interfaces
	: IMPLEMENTS ClassNameList
	  {
		CLIPS *start = $2;
		while(start)
		{
			if(strcmp(start->buffer,",") == 0)
			{
				CLIPS *c = createFromStringWithToken(" public ",IMPLEMENTS);
				c->next = start->next;
				start->next = c;
			}
			start = start->next;
		}
		CLIPS *c = createFromStringWithToken(" public ",IMPLEMENTS);
		$$ = clips_tail_to_head(c,$2);
	  }
	;

FieldDeclarations
	: FieldDeclaration
    | FieldDeclarations FieldDeclaration
    {
		clips_tail_to_head($1,$2);
    }
	;

FieldDeclaration
	: FieldVariableDeclaration ';'
	  {
		CLIPS *c = createFromString(";");
		CLIPS *param = clips_tail_to_head($1,c);
		addClassParam(param);
		$$ = createFromString("");
		de_clips_list(param);
	  }
	| MethodDeclaration
	| ConstructorDeclaration
	{
		addClassParam($1);
		$$ = createFromString("");
		de_clips_list($1);
	}
	| StaticInitializer
    | NonStaticInitializer
    | TypeDeclaration
	;

FieldVariableDeclaration
	: Modifiers TypeSpecifier VariableDeclarators
	  {
		$$ = clips_tail_to_head(clips_tail_to_head($1,$2),$3);
	  }
	|           TypeSpecifier VariableDeclarators
	{
		$$ = clips_tail_to_head($1,$2);
	}
	;

VariableDeclarators
	: VariableDeclarator
	| VariableDeclarators ',' VariableDeclarator
	{
		$$ = clips_tail_to_head($1,$3);
	}
	;

VariableDeclarator
	: DeclaratorName
	| DeclaratorName '=' VariableInitializer
	{
		CLIPS *declar = auditForPointer($1,$3);
		CLIPS *c = createFromString("= ");
		$$ = clips_tail_to_head(declar,clips_tail_to_head(c,$3));
	}
	;

VariableInitializer
	: Expression
	| '{' '}'
	{
		CLIPS *left = createFromString("{");
		CLIPS *right = createFromString("}");
		$$ = clips_tail_to_head(left,right);
	}
	| '{' ArrayInitializers '}'
	{
		CLIPS *left = createFromString("{");
		CLIPS *right = createFromString("}");
		$$ = clips_tail_to_head(clips_tail_to_head(left,$2),right);
	}
	;

ArrayInitializers
	: VariableInitializer
	| ArrayInitializers ',' VariableInitializer
	{
		$$ = clips_tail_to_head($1,$3);
	}
	| ArrayInitializers ','
	;

MethodDeclaration
	: Modifiers TypeSpecifier MethodDeclarator Throws MethodBody
	  {
			
		if(strcmp($3->buffer,"main") == 0)
		{
			$$ = createMain($5);
			de_clips_list($1);
			de_clips_list($2);
			de_clips_list($3);
			de_clips_list($4);
		} else {
			int isStatic = processMethod($1,$2,$3,$4,$5);
			if(isStatic)
			{
				CLIPS *c = createFromStringWithToken("static",STATIC);
				$$ = clips_tail_to_head(c,clips_tail_to_head(clips_tail_to_head($2,$3),$5));
			} else {
			$$ = clips_tail_to_head($2,clips_tail_to_head($3,$5));
			}
			de_clips_list($1);
			de_clips_list($4);
		}
	}
	| Modifiers TypeSpecifier MethodDeclarator        MethodBody
	{
		if(strcmp($3->buffer,"main") == 0)
		{
			$$ = createMain($4);
			de_clips_list($1);
			de_clips_list($2);
			de_clips_list($3);
		} else {
			int isStatic = processMethod($1,$2,$3,0,$4);
			if(isStatic)
			{
				CLIPS *c = createFromStringWithToken("static",STATIC);
				$$ = clips_tail_to_head(c,clips_tail_to_head(clips_tail_to_head($2,$3),$4));
			} else {
				$$ = clips_tail_to_head(clips_tail_to_head($2,$3),$4);
			}
			de_clips_list($1);
		}
	}
	|           TypeSpecifier MethodDeclarator Throws MethodBody
	{
		if(strcmp($3->buffer,"main") == 0)
		{
			$$ = createMain($4);
			de_clips_list($1);
			de_clips_list($2);
			de_clips_list($3);
		} else {
			int isStatic = processMethod(0,$1,$2,$3,$4);
			if(isStatic)
			{
				CLIPS *c = createFromStringWithToken("static",STATIC);
				$$ = clips_tail_to_head(c,clips_tail_to_head(clips_tail_to_head($1,$2),$4));
			} else {
		$$ = clips_tail_to_head($1,clips_tail_to_head($2,$4));
			}
			de_clips_list($3);
		}
	}
	|           TypeSpecifier MethodDeclarator        MethodBody
	{
		if(strcmp($3->buffer,"main") == 0)
		{
			$$ = createMain($3);
			de_clips_list($1);
			de_clips_list($2);
		} else {
			int isStatic = processMethod(0,$1,$2,0,$3);
			if(isStatic)
			{
				CLIPS *c = createFromStringWithToken("static",STATIC);
				$$ = clips_tail_to_head(c,clips_tail_to_head(clips_tail_to_head($1,$2),$3));
			} else {
			$$ = clips_tail_to_head(clips_tail_to_head($1,$2),$3);
			}
		}
	}
	;

MethodDeclarator
	: DeclaratorName '(' ParameterList ')'
	  {
		CLIPS *left = createFromString("(");
		CLIPS *right = createFromString(")");
		$$ = clips_tail_to_head(clips_tail_to_head(clips_tail_to_head($1,left),$3),right);
	  }
	| DeclaratorName '(' ')'
	{
		CLIPS *left = createFromString("(");
		CLIPS *right = createFromString(")");
		$$ = clips_tail_to_head(clips_tail_to_head($1,left),right);
	}
	| MethodDeclarator OP_DIM
	{
		$$ = clips_tail_to_head($1,$2);
	}
	;

ParameterList
	: Parameter
	| ParameterList ',' Parameter
	{
		CLIPS *c = createFromString(",");
		$$ = clips_tail_to_head(clips_tail_to_head($1,c),$3);
	}
	;

Parameter
	: TypeSpecifier DeclaratorName
	  {
		$$ = clips_tail_to_head($1,$2);
	  }
	;

DeclaratorName
	: IDENTIFIER
	| DeclaratorName OP_DIM
	{
		$$ = clips_tail_to_head($1,$2);
	}
	;

Throws
	: THROWS ClassNameList
	  {
		$$ = $2;
		$$->token = THROWS;
	  }
	;

MethodBody
	: Block
	| ';'
	{
		$$ = createFromString(";");
	}
	;

ConstructorDeclaration
	: Modifiers ConstructorDeclarator Throws Block
	  {
		$$ = clips_tail_to_head(clips_tail_to_head(clips_tail_to_head($1,$2),$3),$4);
	  }
	| Modifiers ConstructorDeclarator        Block
	{
		$$ = clips_tail_to_head(clips_tail_to_head($1,$2),$3);
	}
	|           ConstructorDeclarator Throws Block
	{
		$$ = clips_tail_to_head(clips_tail_to_head($1,$2),$3);
	}
	|           ConstructorDeclarator        Block
	{
		$$ = clips_tail_to_head($1,$2);
	}
	;

ConstructorDeclarator
	: IDENTIFIER '(' ParameterList ')'
	  {
		CLIPS *left = createFromString("(");
		CLIPS *right = createFromString(")");
		$$ = clips_tail_to_head(clips_tail_to_head(clips_tail_to_head($1,left),$3),right);
	  }
	| IDENTIFIER '(' ')'
	{
		CLIPS *left = createFromString("(");
		CLIPS *right = createFromString(")");
		$$ = clips_tail_to_head(clips_tail_to_head($1,left),right);
	}
	;

StaticInitializer
	: STATIC Block
	  {
		CLIPS *c = createFromStringWithToken("static",STATIC);
		$$ = clips_tail_to_head(c,$2);
	  }
	;

NonStaticInitializer
        : Block
        ;

Extends
	: EXTENDS TypeName
	  {
		CLIPS *c = createFromString (" public ");
		$$ = clips_tail_to_head(c,$2);
		/*
		$$ = $2;
		$$->token = $1.token;
	  */
	  }
	| Extends ',' TypeName
	{
		CLIPS *c = createFromString (", public ");
		$$ = clips_tail_to_head($1,clips_tail_to_head(c,$3));
		/*
		$3->token = EXTENDS;
		$$ = clips_tail_to_head($1,$3);
		*/
	}
	;

Block
	: '{' LocalVariableDeclarationsAndStatements '}'
	  {
		CLIPS *left = createFromString("{");
		CLIPS *right = createFromString("}");
		$$ = clips_tail_to_head(clips_tail_to_head(left,$2),right);
	  }
	| '{' '}'
	{
		
		CLIPS *left = createFromString("{");
		CLIPS *right = createFromString("}");
		$$ = clips_tail_to_head(left,right);
			
	}
    ;

LocalVariableDeclarationsAndStatements
	: LocalVariableDeclarationOrStatement
	| LocalVariableDeclarationsAndStatements LocalVariableDeclarationOrStatement
	{
		$$ = clips_tail_to_head($1,$2);
	}
	;

LocalVariableDeclarationOrStatement
	: LocalVariableDeclarationStatement
	| Statement
	;

LocalVariableDeclarationStatement
	: Modifiers TypeSpecifier VariableDeclarators ';'
	  {
		CLIPS *c = createFromString(";");
		$$ = clips_tail_to_head(clips_tail_to_head(clips_tail_to_head($1,$2),$3),c);
	  }
	| TypeSpecifier VariableDeclarators ';'
	{
		CLIPS *c = createFromString(";");
		$$ = clips_tail_to_head(clips_tail_to_head($1,$2),c);
	}
	;

Statement
	: EmptyStatement
	| LabeledStatement
	| ExpressionStatement ';'
	{
		CLIPS *c = createFromString(";");
		$$ = clips_tail_to_head($1,c);
	}
        | SelectionStatement
        | IterationStatement
	| JumpStatement
	| GuardingStatement
	| Block
	;

EmptyStatement
	: ';'
	{
		$$ = createFromString(";");
	}
        ;

LabeledStatement
	: IDENTIFIER ':' LocalVariableDeclarationOrStatement
	  {
		CLIPS *c = createFromString(":");
		$$ = clips_tail_to_head(clips_tail_to_head($1,c),$3);
	  }
        | CASE ConstantExpression ':' LocalVariableDeclarationOrStatement
        {
        	CLIPS *c1 = createFromStringWithToken("case",CASE);
        	CLIPS *c2 = createFromString(":");
        	$$ = clips_tail_to_head(clips_tail_to_head(clips_tail_to_head(c1,$1),c2),$4);
        }
	| DEFAULT ':' LocalVariableDeclarationOrStatement
	{
		CLIPS *c1 = createFromStringWithToken("default",DEFAULT);
		CLIPS *c2 = createFromString(":");
		$$ = clips_tail_to_head(clips_tail_to_head(c1,c2),$3);
	}
        ;

ExpressionStatement
	: Expression
	;

SelectionStatement
	: IF '(' Expression ')' Statement
	  {
		CLIPS *c1 = createFromStringWithToken("if",IF);
		CLIPS *left = createFromString("(");
		CLIPS *right = createFromString(")");
		$$ = clips_tail_to_head(clips_tail_to_head(clips_tail_to_head(clips_tail_to_head(c1,left),$3),right),$5);
	  }
        | IF '(' Expression ')' Statement ELSE Statement
        {
        	CLIPS *c1 = createFromStringWithToken("if",IF);
        	CLIPS *left = createFromString("(");
        	CLIPS *right = createFromString(")");
        	CLIPS *c2 = createFromStringWithToken("else",ELSE);
        	$$ = clips_tail_to_head(clips_tail_to_head(clips_tail_to_head(clips_tail_to_head(clips_tail_to_head(clips_tail_to_head(c1,left),$3),right),$5),c2),$7);
        }
        | SWITCH '(' Expression ')' Block
        {
        	CLIPS *c1 = createFromStringWithToken("switch",SWITCH);
        	CLIPS *left = createFromString("(");
        	CLIPS *right = createFromString(")");
        	$$ =   clips_tail_to_head(clips_tail_to_head(clips_tail_to_head(clips_tail_to_head(c1,left),$3),right),$5);
        }
        ;

IterationStatement
	: WHILE '(' Expression ')' Statement
	  {
		CLIPS *c1 = createFromStringWithToken("while",WHILE);
		CLIPS *left = createFromString("(");
		CLIPS *right = createFromString(")");
		$$ = clips_tail_to_head(clips_tail_to_head(clips_tail_to_head(clips_tail_to_head(c1,left),$3),right),$5);
	 }
	| DO Statement WHILE '(' Expression ')' ';'
	{
		CLIPS *c1 = createFromStringWithToken("do",DO);
		CLIPS *c2 = createFromStringWithToken("while",WHILE);
		CLIPS *c3 = createFromString(";");
		CLIPS *left = createFromString("(");
		CLIPS *right = createFromString(")");
		$$ =   clips_tail_to_head(clips_tail_to_head(clips_tail_to_head(clips_tail_to_head(clips_tail_to_head(clips_tail_to_head(c1,$2),c2),left),$5),right),c3);
	}
	| FOR '(' ForInit ForExpr ForIncr ')' Statement
	{
		CLIPS *c1 = createFromStringWithToken("for",FOR);
		CLIPS *left = createFromString("(");
		CLIPS *right = createFromString(")");
		maskClips($3,MASK_NOLF);
		maskClips($4,MASK_NOLF);
		maskClips($5,MASK_NOLF);
		$$ = clips_tail_to_head(clips_tail_to_head(clips_tail_to_head(clips_tail_to_head(clips_tail_to_head(clips_tail_to_head(c1,left),$3),$4),$5),right),$7);
	}
	| FOR '(' ForInit ForExpr         ')' Statement
	{
		CLIPS *c1 = createFromStringWithToken("for",FOR);
		CLIPS *left = createFromString("(");
		CLIPS *right = createFromString(")");
		maskClips($3,MASK_NOLF);
		maskClips($4,MASK_NOLF);
		$$ = clips_tail_to_head(clips_tail_to_head(clips_tail_to_head(clips_tail_to_head(clips_tail_to_head(c1,left),$3),$4),right),$6);
	}
	;

ForInit
	: ExpressionStatements ';'
	  {
		CLIPS *c = createFromString(";");
		$$ = clips_tail_to_head($1,c);
	  }
	| LocalVariableDeclarationStatement
	| ';'
	{
		$$ = createFromString(";");
	}
	;

ForExpr
	: Expression ';'
	  {
		CLIPS *c = createFromString(";");
		$$ = clips_tail_to_head($1,c);
	  }
	| ';'
	{
		$$ = createFromString(";");
	}
	;

ForIncr
	: ExpressionStatements
	;

ExpressionStatements
	: ExpressionStatement
	| ExpressionStatements ',' ExpressionStatement
	{
		CLIPS *c = createFromString(",");
		$$ = clips_tail_to_head(clips_tail_to_head($1,c),$3);
	}
	;

JumpStatement
	: BREAK IDENTIFIER ';'
	  {
		CLIPS *c1 = createFromStringWithToken("break",BREAK);
		CLIPS *c2 = createFromString(";");
		$$ = clips_tail_to_head(clips_tail_to_head(c1,$2),c2);
	  }
	| BREAK            ';'
	{
		CLIPS *c1 = createFromStringWithToken("break",BREAK);
		CLIPS *c2 = createFromString(";");
		$$ = clips_tail_to_head(c1,c2);
	}
    | CONTINUE IDENTIFIER ';'
    {
		CLIPS *c1 = createFromStringWithToken("continue",CONTINUE);
		CLIPS *c2 = createFromString(";");
		$$ = clips_tail_to_head(clips_tail_to_head(c1,$2),c2);
    }
	| CONTINUE            ';'
	{
		CLIPS *c1 = createFromStringWithToken("continue",CONTINUE);
		CLIPS *c2 = createFromString(";");
		$$ = clips_tail_to_head(c1,c2);
		}
	| RETURN Expression ';'
	{
		CLIPS *c1 = createFromStringWithToken("return",RETURN);
		CLIPS *c2 = createFromString(";");
		$$ = clips_tail_to_head(clips_tail_to_head(c1,$2),c2);
	}
	| RETURN            ';'
	{
		CLIPS *c1 = createFromStringWithToken("return",RETURN);
		CLIPS *c2 = createFromString(";");
		$$ = clips_tail_to_head(c1,c2);
	}
	| THROW Expression ';'
	{
		CLIPS *c1 = createFromStringWithToken("throw",THROW);
		CLIPS *c2 = createFromString(";");
		$$ = clips_tail_to_head(clips_tail_to_head(c1,$2),c2);
	}
	;

GuardingStatement
	: SYNCHRONIZED '(' Expression ')' Statement
	  {
		CLIPS *c1 = createFromStringWithToken("synchronized",SYNCHRONIZED);
		CLIPS *left = createFromString("(");
		CLIPS *right = createFromString(")");
		$$ = clips_tail_to_head(clips_tail_to_head(clips_tail_to_head(clips_tail_to_head(c1,left),$3),right),$5);
	  }
	| TRY Block Finally
	{
		CLIPS *c1 = createFromStringWithToken("try",TRY);
		$$ = clips_tail_to_head(clips_tail_to_head(c1,$2),$3);
	}
	| TRY Block Catches
	{
		CLIPS *c1 = createFromStringWithToken("try",TRY);
		$$ = clips_tail_to_head(clips_tail_to_head(c1,$2),$3);
	}
	| TRY Block Catches Finally
	{
		CLIPS *c1 = createFromStringWithToken("try",TRY);
		$$ = clips_tail_to_head(clips_tail_to_head(clips_tail_to_head(c1,$2),$3),$4);
	}
	;

Catches
	: Catch
	| Catches Catch
	{
		$$ = clips_tail_to_head($1,$2);
	}
	;

Catch
	: CatchHeader Block
	  {
		$$ = clips_tail_to_head($1,$2);
	  }
	;

CatchHeader
	: CATCH '(' TypeSpecifier IDENTIFIER ')'
	  {
	  		CLIPS *c1 = createFromStringWithToken("catch",CATCH);
	  		CLIPS *left = createFromString("(");
	  		CLIPS *right = createFromString(")");
	  		$$ = clips_tail_to_head(clips_tail_to_head(clips_tail_to_head(clips_tail_to_head(c1,left),$3),$4),right);
	  	}
	| CATCH '(' TypeSpecifier ')'
	{
		CLIPS *c1 = createFromStringWithToken("catch",CATCH);
		CLIPS *left = createFromString("(");
		CLIPS *right = createFromString(")");
		$$ = clips_tail_to_head(clips_tail_to_head(clips_tail_to_head(c1,left),$3),right);
	}
	;

Finally
	: FINALLY Block
	  {
		CLIPS *c1 = createFromStringWithToken("finally",FINALLY);
		$$ = clips_tail_to_head(c1,$2);
	  }
	;

PrimaryExpression
	: QualifiedName
	| NotJustName
	;

NotJustName
	: SpecialName
	| NewAllocationExpression
	| ComplexPrimary
	;

ComplexPrimary
	: '(' Expression ')'
	  {
		CLIPS *left = createFromString("(");
		CLIPS *right = createFromString(")");
		$$ = clips_tail_to_head(clips_tail_to_head(left,$2),right);
	  }
	| ComplexPrimaryNoParenthesis
	;

ComplexPrimaryNoParenthesis
	: LITERAL
	| BOOLLIT
	| ArrayAccess
	| FieldAccess
	| MethodCall
	;

ArrayAccess
	: QualifiedName '[' Expression ']'
	  {
		CLIPS *left = createFromString("[");
		CLIPS *right = createFromString("]");
		$$ = clips_tail_to_head(clips_tail_to_head(clips_tail_to_head($1,left),$3),right);
	  }
	| ComplexPrimary '[' Expression ']'
	{
			CLIPS *left = createFromString("[");
			CLIPS *right = createFromString("]");
			$$ = clips_tail_to_head(clips_tail_to_head(clips_tail_to_head($1,left),$3),right);
		  }
	;

FieldAccess
	: NotJustName '.' IDENTIFIER
	{
		  CLIPS *c = createFromString(".");
		  $$ = clips_tail_to_head(clips_tail_to_head($1,c),$3);
		  
	}
	| RealPostfixExpression '.' IDENTIFIER
	{
		  CLIPS *c = createFromString(".");
		  $$ = clips_tail_to_head(clips_tail_to_head($1,c),$3);
	}
	;

MethodCall
	: MethodAccess '(' ArgumentList ')'
	  {
		if(strcmp($1->buffer,"System.out.println") == 0 || strcmp($1->buffer,"System.out.print") == 0 )
		{
			$$ = processPrintLine($3);
			de_clips_list($1);
		} else {
			CLIPS *left = createFromString("(");
			CLIPS *right = createFromString(")");
			$$ = clips_tail_to_head(clips_tail_to_head(clips_tail_to_head($1,left),$3),right);
		}
	}
	| MethodAccess '(' ')'
	{
		CLIPS *left = createFromString("(");
		CLIPS *right = createFromString(")");
		$$ = clips_tail_to_head(clips_tail_to_head($1,left),right);
	}
	;

MethodAccess
	: ComplexPrimaryNoParenthesis
	| SpecialName
	| QualifiedName
	;

SpecialName
	: THIS
	| SUPER
	| JNULL
	;

ArgumentList
	: Expression
	| ArgumentList ',' Expression
	{
		CLIPS *c = createFromString(",");
		$$ = clips_tail_to_head(clips_tail_to_head($1,c),$3);
	}
	;

NewAllocationExpression
    	: ArrayAllocationExpression
    	| ClassAllocationExpression
    	| ArrayAllocationExpression '{' '}'
    	{
    		CLIPS *left = createFromString("{");
    		CLIPS *right = createFromString("}");
    		$$ = clips_tail_to_head(clips_tail_to_head($1,left),right);
    	}
    	| ClassAllocationExpression '{' '}'
    	{
			CLIPS *left = createFromString("{");
			CLIPS *right = createFromString("}");
			$$ = clips_tail_to_head(clips_tail_to_head($1,left),right);
    	}
    	| ArrayAllocationExpression '{' ArrayInitializers '}'
    	{
    		CLIPS *left = createFromString("{");
    		CLIPS *right = createFromString("}");
    		$$ = clips_tail_to_head(clips_tail_to_head(clips_tail_to_head($1,left),$3),right);
    	}
    	| ClassAllocationExpression '{' FieldDeclarations '}'
    	{
			CLIPS *left = createFromString("{");
			CLIPS *right = createFromString("}");
			$$ = clips_tail_to_head(clips_tail_to_head(clips_tail_to_head($1,left),$3),right);
    	}
    	;

ClassAllocationExpression
	: NEW TypeName '(' ArgumentList ')'
	  {
		CLIPS *c = createFromStringWithToken("new",NEW);
		CLIPS *left = createFromString("(");
		CLIPS *right = createFromString(")");
		$$ = clips_tail_to_head(clips_tail_to_head(clips_tail_to_head(clips_tail_to_head(c,$2),left),$4),right);
	  }
	| NEW TypeName '('              ')'
	{
		CLIPS *c = createFromStringWithToken("new",NEW);
		CLIPS *left = createFromString("(");
		CLIPS *right = createFromString(")");
		$$ = clips_tail_to_head(clips_tail_to_head(clips_tail_to_head(c,$2),left),right);
	}
        ;

ArrayAllocationExpression
	: NEW TypeName DimExprs Dims
	  {
		CLIPS *c = createFromStringWithToken("new",NEW);
		$$ = clips_tail_to_head(clips_tail_to_head(clips_tail_to_head(c,$2),$3),$4);
	  }
	| NEW TypeName DimExprs
	{
		CLIPS *c = createFromStringWithToken("new",NEW);
		$$ = clips_tail_to_head(clips_tail_to_head(c,$2),$3);
	}
        | NEW TypeName Dims
        {
        	CLIPS *c = createFromStringWithToken("new",NEW);
        	$$ = clips_tail_to_head(clips_tail_to_head(c,$2),$3);
        }
	;

DimExprs
	: DimExpr
	| DimExprs DimExpr
	{
		$$ = clips_tail_to_head($1,$2);
	}
	;

DimExpr
	: '[' Expression ']'
	  {
		CLIPS *left = createFromString("[");
		CLIPS *right = createFromString("]");
		$$ = clips_tail_to_head(clips_tail_to_head(left,$2),right);
	  }
	;

Dims
	: OP_DIM
	  {
		$$ = $1;
		$$->token = OP_DIM;
	  }
	| Dims OP_DIM
	{
		CLIPS *c = $2;
		c->token = OP_DIM;
		$$ = clips_tail_to_head($1,c);
	}
	;

PostfixExpression
	: PrimaryExpression
	| RealPostfixExpression
	;

RealPostfixExpression
	: PostfixExpression OP_INC
	  {
		CLIPS *c = createFromStringWithToken("++",OP_INC);
		$$ = clips_tail_to_head($1,c);
	  }
	| PostfixExpression OP_DEC
	{
		CLIPS *c = createFromStringWithToken("--",OP_DEC);
		$$ = clips_tail_to_head($1,c);
	}
	;

UnaryExpression
	: OP_INC UnaryExpression
	  {
		CLIPS *c = createFromStringWithToken("++",OP_INC);
		$$ = clips_tail_to_head(c,$2);
	  }
	| OP_DEC UnaryExpression
	{
		CLIPS *c = createFromStringWithToken("--",OP_DEC);
		$$ = clips_tail_to_head(c,$2);
	}
	| ArithmeticUnaryOperator CastExpression
	{
		$$ = clips_tail_to_head($1,$2);
	}
	| LogicalUnaryExpression
	;

LogicalUnaryExpression
	: PostfixExpression
	| LogicalUnaryOperator UnaryExpression
	{
		$$ = clips_tail_to_head($1,$2);
	}
	;

LogicalUnaryOperator
	: '~'
	{
		$$ = createFromString("~");
	}	
	| '!'
	{
		$$ = createFromString("!");
	}
	;

ArithmeticUnaryOperator
	: '+'
	{
		$$ = createFromString("+");
	}	
	| '-'
	{
		$$ = createFromString("-");
	}
	;

CastExpression
	: UnaryExpression
	| '(' PrimitiveTypeExpression ')' CastExpression
	{
		CLIPS *left = createFromString("(");
		CLIPS *right = createFromString(")");
		$$ = clips_tail_to_head(clips_tail_to_head(clips_tail_to_head(left,$2),right),$4);
	}
	| '(' ClassTypeExpression ')' CastExpression
	{
			CLIPS *left = createFromString("(");
			CLIPS *right = createFromString(")");
			$$ = clips_tail_to_head(clips_tail_to_head(clips_tail_to_head(left,$2),right),$4);
		}
	| '(' Expression ')' LogicalUnaryExpression
	{
			CLIPS *left = createFromString("(");
			CLIPS *right = createFromString(")");
			$$ = clips_tail_to_head(clips_tail_to_head(clips_tail_to_head(left,$2),right),$4);
		}
	;

PrimitiveTypeExpression
	: PrimitiveType
        | PrimitiveType Dims
        {
		$$ = clips_tail_to_head($1,$2);
        }
        ;

ClassTypeExpression
	: QualifiedName Dims
	  {
		$$ = clips_tail_to_head($1,$2);
	  }
    ;

MultiplicativeExpression
	: CastExpression
	| MultiplicativeExpression '*' CastExpression
	{
		CLIPS *c1 = createFromString("*");
		$$ =  clips_tail_to_head(clips_tail_to_head($1,c1),$3);
	}
	| MultiplicativeExpression '/' CastExpression
	{
		CLIPS *c1 = createFromString("/");
		$$ =  clips_tail_to_head(clips_tail_to_head($1,c1),$3);
	}
	| MultiplicativeExpression '%' CastExpression
	{
		CLIPS *c1 = createFromString("%");
		$$ =  clips_tail_to_head(clips_tail_to_head($1,c1),$3);
	}
	;

AdditiveExpression
	: MultiplicativeExpression
        | AdditiveExpression '+' MultiplicativeExpression
        {
			CLIPS *c1 = createFromString("+");
			$$ =  clips_tail_to_head(clips_tail_to_head($1,c1),$3);
        }
        | AdditiveExpression '-' MultiplicativeExpression
        {
        	CLIPS *c1 = createFromString("-");
        	$$ =  clips_tail_to_head(clips_tail_to_head($1,c1),$3);
        }
        ;

ShiftExpression
	: AdditiveExpression
        | ShiftExpression OP_SHL AdditiveExpression
        {
		CLIPS *c1 = createFromStringWithToken("<<=",OP_SHL);
		$$ =  clips_tail_to_head(clips_tail_to_head($1,c1),$3);
        }
        | ShiftExpression OP_SHR AdditiveExpression
        {
		CLIPS *c1 = createFromStringWithToken(">>=",OP_SHR);
		$$ =  clips_tail_to_head(clips_tail_to_head($1,c1),$3);
        }
        | ShiftExpression OP_SHRR AdditiveExpression
        {
			CLIPS *c1 = createFromStringWithToken(">>>=",OP_SHRR);
			$$ =  clips_tail_to_head(clips_tail_to_head($1,c1),$3);
        }
	;

RelationalExpression
	: ShiftExpression
        | RelationalExpression '<' ShiftExpression
        {
		CLIPS *c1 = createFromString("<");
		$$ =  clips_tail_to_head(clips_tail_to_head($1,c1),$3);
        }
	| RelationalExpression '>' ShiftExpression
	{
		CLIPS *c1 = createFromString(">");
		$$ =  clips_tail_to_head(clips_tail_to_head($1,c1),$3);
	}
	| RelationalExpression OP_LE ShiftExpression
	{
		CLIPS *c1 = createFromStringWithToken("<=",OP_LE);
		$$ =  clips_tail_to_head(clips_tail_to_head($1,c1),$3);
	}
	| RelationalExpression OP_GE ShiftExpression
	{
		CLIPS *c1 = createFromStringWithToken(">=",OP_GE);
		$$ =  clips_tail_to_head(clips_tail_to_head($1,c1),$3);
	}
	| RelationalExpression INSTANCEOF TypeSpecifier
	{
		CLIPS *c1 = createFromStringWithToken("instanceof",INSTANCEOF);
		$$ =  clips_tail_to_head(clips_tail_to_head($1,c1),$3);
	}
	;

EqualityExpression
	: RelationalExpression
        | EqualityExpression OP_EQ RelationalExpression
        {
			CLIPS *c1 = createFromStringWithToken("==",OP_EQ);
			$$ =  clips_tail_to_head(clips_tail_to_head($1,c1),$3);
        }
        | EqualityExpression OP_NE RelationalExpression
        {
        	CLIPS *c1 = createFromStringWithToken("!=",OP_NE);
        	$$ =  clips_tail_to_head(clips_tail_to_head($1,c1),$3);
        }
        ;

AndExpression
	: EqualityExpression
        | AndExpression '&' EqualityExpression
        {
			CLIPS *c1 = createFromString("&");
			$$ =  clips_tail_to_head(clips_tail_to_head($1,c1),$3);
        }
        ;

ExclusiveOrExpression
	: AndExpression
	| ExclusiveOrExpression '^' AndExpression
	{
		CLIPS *c1 = createFromString("^");
		$$ =  clips_tail_to_head(clips_tail_to_head($1,c1),$3);
	}
	;

InclusiveOrExpression
	: ExclusiveOrExpression
	| InclusiveOrExpression '|' ExclusiveOrExpression
	{
	CLIPS *c1 = createFromString("|");
	$$ =  clips_tail_to_head(clips_tail_to_head($1,c1),$3);
	}
	;

ConditionalAndExpression
	: InclusiveOrExpression
	| ConditionalAndExpression OP_LAND InclusiveOrExpression
	{
		CLIPS *c1 = createFromStringWithToken("&&",OP_LAND);
		$$ =  clips_tail_to_head(clips_tail_to_head($1,c1),$3);
	}
	;

ConditionalOrExpression
	: ConditionalAndExpression
	| ConditionalOrExpression OP_LOR ConditionalAndExpression
	{
		CLIPS *c1 = createFromStringWithToken("||",OP_LOR);
		$$ =  clips_tail_to_head(clips_tail_to_head($1,c1),$3);
	}
	;

ConditionalExpression
	: ConditionalOrExpression
	| ConditionalOrExpression '?' Expression ':' ConditionalExpression
	{
		CLIPS *c1 = createFromString("?");
		CLIPS *c2 = createFromString(":");
		$$ = clips_tail_to_head(clips_tail_to_head(clips_tail_to_head(clips_tail_to_head($1,c1),$3),c2),$5);
	}
	;

AssignmentExpression
	: ConditionalExpression
	| UnaryExpression AssignmentOperator AssignmentExpression
	{
		$$ = clips_tail_to_head(clips_tail_to_head($1,$2),$3);
	}
	;

AssignmentOperator
	: '='
	{
		$$ = createFromString("=");
	}	
	| ASS_MUL
	{
		$$ = createFromStringWithToken("*=",ASS_MUL);
	}
	| ASS_DIV
	{
		$$ = createFromStringWithToken("/=",ASS_DIV);
	}
	| ASS_MOD
	{
		$$ = createFromStringWithToken("%=",ASS_MOD);
	}
	| ASS_ADD
	{
		$$ = createFromStringWithToken("+=",ASS_ADD);
	}
	| ASS_SUB
	{
		$$ = createFromStringWithToken("-=",ASS_SUB);
	}
	| ASS_SHL
	{
		$$ = createFromStringWithToken("<<=",ASS_SHL);
	}
	| ASS_SHR
	{
		$$ = createFromStringWithToken(">>=",ASS_SHR);
	}
	| ASS_SHRR
	{
		$$ = createFromStringWithToken(">>>=",ASS_SHRR);
	}
	| ASS_AND
	{
		$$ = createFromStringWithToken("&=",ASS_AND);
	}
	| ASS_XOR
	{
		$$ = createFromStringWithToken("^=",ASS_XOR);
	}
	| ASS_OR
	{
		$$ = createFromStringWithToken("|=",ASS_OR);
	}
	;

Expression
	: AssignmentExpression
    ;

ConstantExpression
	: ConditionalExpression
	;

%%

void	yyerror( char *s)
{
	printf("\n%*s\n%*s\n", data.column, "^", data.column, s);
}


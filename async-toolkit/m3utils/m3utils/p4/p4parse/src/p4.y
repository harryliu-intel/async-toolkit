p4program
    :
    | p4program declaration
    | p4program ';'  
    ;

declaration
    : constantDeclaration
    | externDeclaration
    | actionDeclaration
    | parserDeclaration
    | typeDeclaration
    | controlDeclaration
    | instantiation
    | errorDeclaration
    | matchKindDeclaration
    | functionDeclaration
    ;

nonTypeName
    : IDENTIFIER
    | APPLY
    | KEY
    | ACTIONS
    | STATE
    | ENTRIES
    | TYPE
    ;

name
    : nonTypeName
    | TYPE_IDENTIFIER
    ;

nonTableKwName
   : IDENTIFIER
   | TYPE_IDENTIFIER
   | APPLY
   | STATE
   | TYPE
   ;

optAnnotations
    : 
    | annotations
    ;

annotations
    : annotation
    | annotations annotation
    ;

annotation
    : '@' name
    | '@' name '(' expressionList ')'
    | '@' name '(' keyValueList ')'
    ;

parameterList
    : 
    | nonEmptyParameterList
    ;

nonEmptyParameterList
    : parameter
    | nonEmptyParameterList ',' parameter
    ;

parameter
    : optAnnotations direction typeRef name
    | optAnnotations direction typeRef name '=' expression
    ;

direction
    : IN
    | OUT
    | INOUT
    | 
    ;

packageTypeDeclaration
    : optAnnotations PACKAGE name optTypeParameters
      '(' parameterList ')'
    ;

instantiation
    : typeRef '(' argumentList ')' name ';'
    | annotations typeRef '(' argumentList ')' name ';'
    ;

optConstructorParameters
    : 
    | '(' parameterList ')'
    ;

dotPrefix
    : '.'
    ;

parserDeclaration
    : parserTypeDeclaration optConstructorParameters
      '{' parserLocalElements parserStates '}'
    ;

parserLocalElements
    :
    | parserLocalElements parserLocalElement
    ;

parserLocalElement
    : constantDeclaration
    | variableDeclaration
    | instantiation
    | valueSetDeclaration
    ;

parserTypeDeclaration
    : optAnnotations PARSER name optTypeParameters '(' parameterList ')'
    ;

parserStates
    : parserState
    | parserStates parserState
    ;

parserState
    : optAnnotations STATE name '{' parserStatements transitionStatement '}'
    ;

parserStatements
    :
    | parserStatements parserStatement
    ;

parserStatement
    : assignmentOrMethodCallStatement
    | directApplication
    | parserBlockStatement
    | constantDeclaration
    | variableDeclaration
    ;

parserBlockStatement
    : optAnnotations '{' parserStatements '}'
    ;

transitionStatement
    :
    | TRANSITION stateExpression
    ;

stateExpression
    : name ';'
    | selectExpression
    ;

selectExpression
    : SELECT '(' expressionList ')' '{' selectCaseList '}'
    ;

selectCaseList
    :
    | selectCaseList selectCase
    ;

selectCase
    : keysetExpression ':' name ';'
    ;

keysetExpression
    : tupleKeysetExpression
    | simpleKeysetExpression
    ;

tupleKeysetExpression
    : '(' simpleKeysetExpression ',' simpleExpressionList ')'
    ;

simpleExpressionList
    : simpleKeysetExpression
    | simpleExpressionList ',' simpleKeysetExpression
    ;

simpleKeysetExpression
    : expression
    | DEFAULT
    | DONTCARE
    | expression MASK expression
    | expression RANGE expression
    ;

valueSetDeclaration
  : optAnnotations
      VALUESET '<' baseType '>' '(' expression ')' name ';'
  | optAnnotations
      VALUESET '<' tupleType '>' '(' expression ')' name ';'
  | optAnnotations
      VALUESET '<' typeName '>' '(' expression ')' name ';'
  ;

controlDeclaration
    : controlTypeDeclaration optConstructorParameters
      '{' controlLocalDeclarations APPLY controlBody '}'
    ;

controlTypeDeclaration
    : optAnnotations CONTROL name optTypeParameters
      '(' parameterList ')'
    ;

controlLocalDeclarations
    :
    | controlLocalDeclarations controlLocalDeclaration
    ;

controlLocalDeclaration
    : constantDeclaration
    | actionDeclaration
    | tableDeclaration
    | instantiation
    | variableDeclaration
    ;

controlBody
    : blockStatement
    ;

externDeclaration
    : optAnnotations EXTERN nonTypeName optTypeParameters '{' methodPrototypes '}'
    | optAnnotations EXTERN functionPrototype ';'
    ;

methodPrototypes
    :
    | methodPrototypes methodPrototype
    ;

functionPrototype
    : typeOrVoid name optTypeParameters '(' parameterList ')'
    ;

methodPrototype
    : optAnnotations functionPrototype ';'
    | optAnnotations TYPE_IDENTIFIER '(' parameterList ')' ';'
    ;

typeRef
    : baseType
    | typeName
    | specializedType
    | headerStackType
    | tupleType
    ;

namedType
    : typeName
    | specializedType
    ;

prefixedType
    : TYPE_IDENTIFIER
    | dotPrefix TYPE_IDENTIFIER
    ;

typeName
    : prefixedType
    ;

tupleType
    : TUPLE '<' typeArgumentList '>'
    ;

headerStackType
    : typeName '[' expression ']'
    ;

specializedType
    : prefixedType '<' typeArgumentList '>'
    ;

baseType
    : BOOL
    | ERROR
    | BIT
    | BIT '<' INTEGER '>'
    | INT '<' INTEGER '>'
    | VARBIT '<' INTEGER '>'
    ;

typeOrVoid
    : typeRef
    | VOID
    | IDENTIFIER    
    ;

optTypeParameters
    :
    | '<' typeParameterList '>'
    ;

typeParameterList
    : name
    | typeParameterList ',' name
    ;

realTypeArg
    : DONTCARE
    | typeRef
    ;

typeArg
    : DONTCARE
    | typeRef
    | nonTypeName
    ;

realTypeArgumentList
    : realTypeArg
    | realTypeArgumentList COMMA typeArg
    ;

typeArgumentList
    : typeArg
    | typeArgumentList ',' typeArg
    ;

typeDeclaration
    : derivedTypeDeclaration
    | typedefDeclaration
    | parserTypeDeclaration ';'
    | controlTypeDeclaration ';'
    | packageTypeDeclaration ';'
    ;

derivedTypeDeclaration
    : headerTypeDeclaration
    | headerUnionDeclaration
    | structTypeDeclaration
    | enumDeclaration
    ;

headerTypeDeclaration
    : optAnnotations HEADER name '{' structFieldList '}'
    ;

headerUnionDeclaration
    : optAnnotations HEADER_UNION name '{' structFieldList '}'
    ;

structTypeDeclaration
    : optAnnotations STRUCT name '{' structFieldList '}'
    ;

structFieldList
    :
    | structFieldList structField
    ;

structField
    : optAnnotations typeRef name ';'
    ;

enumDeclaration
    : optAnnotations ENUM name '{' identifierList '}'
    | optAnnotations ENUM BIT '<' INTEGER '>' name '{' specifiedIdentifierList '}'
    ;

errorDeclaration
    : ERROR '{' identifierList '}'
    ;

matchKindDeclaration
    : MATCH_KIND '{' identifierList '}'
    ;

identifierList
    : name
    | identifierList ',' name
    ;

specifiedIdentifierList
    : specifiedIdentifier
    | specifiedIdentifierList ',' specifiedIdentifier
    ;

specifiedIdentifier
    : name '=' initializer
    ;

typedefDeclaration
    : optAnnotations TYPEDEF typeRef name ';'
    | optAnnotations TYPEDEF derivedTypeDeclaration name ';'
    | optAnnotations TYPE typeRef name ';'
    | optAnnotations TYPE derivedTypeDeclaration name ';'
    ;

assignmentOrMethodCallStatement
    : lvalue '(' argumentList ')' ';'
    | lvalue '<' typeArgumentList '>' '(' argumentList ')' ';'
    | lvalue '='  expression ';'
    ;

emptyStatement
    : ';'
    ;

returnStatement
    : RETURN ';'
    | RETURN expression ';'
    ;

exitStatement
    : EXIT ';'
    ;

conditionalStatement
    : IF '(' expression ')' statement
    | IF '(' expression ')' statement ELSE statement
    ;

directApplication
    : typeName '.' APPLY '(' argumentList ')' ';'
    ;

statement
    : assignmentOrMethodCallStatement
    | directApplication
    | conditionalStatement
    | emptyStatement
    | blockStatement
    | exitStatement
    | returnStatement
    | switchStatement
    ;

blockStatement
    : optAnnotations '{' statOrDeclList '}'
    ;

statOrDeclList
    : 
    | statOrDeclList statementOrDeclaration
    ;

switchStatement
    : SWITCH '(' expression ')' '{' switchCases '}'
    ;

switchCases
    : 
    | switchCases switchCase
    ;

switchCase
    : switchLabel ':' blockStatement
    | switchLabel ':'
    ;

switchLabel
    : name
    | DEFAULT
    ;

statementOrDeclaration
    : variableDeclaration
    | constantDeclaration
    | statement
    | instantiation
    ;

tableDeclaration
    : optAnnotations TABLE name '{' tablePropertyList '}'
    ;

tablePropertyList
    : tableProperty
    | tablePropertyList tableProperty
    ;

tableProperty
    : KEY '=' '{' keyElementList '}'
    | ACTIONS '=' '{' actionList '}'
    | CONST ENTRIES '=' '{' entriesList '}' 
    | optAnnotations CONST nonTableKwName '=' initializer ';'
    | optAnnotations nonTableKwName '=' initializer ';'
    ;

keyElementList
    : 
    | keyElementList keyElement
    ;

keyElement
    : expression ':' name optAnnotations ';'
    ;

actionList
    : 
    | actionList actionRef ';'
    ;

entriesList
    : entry
    | entriesList entry
    ;

entry
    : keysetExpression ':' actionRef optAnnotations ';'
    ;

actionRef
    : optAnnotations name
    | optAnnotations name '(' argumentList ')'
    ;

actionDeclaration
    : optAnnotations ACTION name '(' parameterList ')' blockStatement
    ;

variableDeclaration
    : annotations typeRef name optInitializer ';'
    | typeRef name optInitializer ';'
    ;

constantDeclaration
    : optAnnotations CONST typeRef name '=' initializer ';'
    ;

optInitializer
    : 
    | '=' initializer
    ;

initializer
    : expression
    ;

functionDeclaration
    : functionPrototype blockStatement
    ;

argumentList
    : 
    | nonEmptyArgList
    ;

nonEmptyArgList
    : argument
    | nonEmptyArgList ',' argument
    ;

argument
    : expression
    | name '=' expression
    | DONTCARE
    ;

expressionList
    : 
    | expression
    | expressionList ',' expression
    ;

keyValuePair
    : IDENTIFIER '=' expression
    ;

keyValueList
    : keyValuePair
    | keyValueList ',' keyValuePair
    ;

member
    : name
    ;

prefixedNonTypeName
    : nonTypeName
    | dotPrefix nonTypeName
    ;

lvalue
    : prefixedNonTypeName
    | lvalue '.' member
    | lvalue '[' expression ']'
    | lvalue '[' expression ':' expression ']'
    ;

%left ','
%nonassoc '?'
%nonassoc ':'
%left OR
%left AND
%left EQ NE
%left '<' '>' LE GE
%left '|'
%left '^'
%left '&'
%left SHL
%left PP '+' '-' '|+|' '|-|'
%left '*' '/' '%'
%right PREFIX
%nonassoc ']' '(' '['
%left '.'

expression
    : INTEGER
    | TRUE
    | FALSE
    | STRING_LITERAL
    | nonTypeName
    | dotPrefix nonTypeName
    | expression '[' expression ']'
    | expression '[' expression ':' expression ']'
    | '{' expressionList '}'
    | '(' expression ')'
    | '!' expression
    | '~' expression
    | '-' expression
    | '+' expression
    | typeName '.' member
    | ERROR '.' member
    | expression '.' member
    | expression '*' expression
    | expression '/' expression
    | expression '%' expression
    | expression '+' expression
    | expression '-' expression
    | expression '|+|' expression
    | expression '|-|' expression
    | expression SHL expression        
    | expression '>''>' expression     
    | expression LE expression         
    | expression GE expression         
    | expression '<' expression
    | expression '>' expression
    | expression NE expression         
    | expression EQ expression         
    | expression '&' expression
    | expression '^' expression
    | expression '|' expression
    | expression PP expression         
    | expression AND expression        
    | expression OR expression         
    | expression '?' expression ':' expression
    | expression '<' realTypeArgumentList '>' '(' argumentList ')'
    | expression '(' argumentList ')'
    | namedType '(' argumentList ')'
    | '(' typeRef ')' expression
    ;

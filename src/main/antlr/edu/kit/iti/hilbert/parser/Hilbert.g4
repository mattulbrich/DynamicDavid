// define a grammar called Hello
grammar Hilbert;

@header {
package edu.kit.iti.hilbert.parser;
}

oneCommand :
  command EOF ;

commandList :
  command+ EOF ;

command :
    theorem
  | instantiate
  | push
  | pop
  | mp
  | assume
  | controls
  ;

controls :
    'facts' # Facts
  | 'fact' id=(ID|FORMULA_ID) # Fact
  | 'quit' # Quit
  | 'clear' ( all='all' )? # Clear
  | 'load' STRING_LIT ( 'unless' ID )? # Load
  ;


instantiate :
  'inst' id=ID
  ( 'with' vars+=ID '=' exps+=formula ( ',' vars+=ID '=' exps+=formula )* )?
  ( 'as' name=ID )?
  ;

push :
  'push' formula 'from' id=(ID | FORMULA_ID)
  ( 'as' name=ID )?
  ;

pop :
  'pop' 'from' id=(ID | FORMULA_ID)
  ( 'as' name=ID )?
  ;

mp :
  'mp' first=(ID | FORMULA_ID) 'and' second=(ID | FORMULA_ID)
  ( 'as' name=ID )?
  ;

theorem :
  'thm' formula 'as' name=ID
  'proof'
  command*
  'end'
  ;

assume :
  'assume' formula ( 'as' name=ID )?
  ;

formula :
    ID   #Id
  | '-' formula #Neg
  | '[' program ']' formula #Box
  | '<' program '>' formula #Diamond
  | '(' formula ')' #Paren
  | <assoc=right> formula '->'  formula #Imp
  ;

program :
    ID #Atomic
  | program ';' program #Seq
  | program '*' #Kleene
  | program '+' program #Choice
  | '?' formula #Test
  | '(' program ')' #ParenProgram
  ;

ID  : [A-Za-z_][A-Za-z0-9$_]* ;
WS : [ \t\r\n]+ -> skip ;
COMMENT : '%' .*? '\n' -> skip ;
FORMULA_ID : '#' [0-9]+ ;
STRING_LIT : '"' .*? '"' ;
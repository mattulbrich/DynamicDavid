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
  | gen
  | assume
  | controls
  ;

controls :
    'facts' # ShowFacts
  | 'fact' id=(ID|FORMULA_ID) ( 'with' proof='proof' )?  # ShowFact
  | 'quit' # Quit
  | 'help' ( name=ID )? # Help
  | 'clear' ( name=ID )? # Clear
  | 'load' STRING_LIT ( 'unless' ID )? # Load
  | 'set' name=ID '=' value=(ID|STRING_LIT) # Set
  ;


instantiate :
  'inst' id=(ID | FORMULA_ID)
  'with'
     ( vars+=ID '=' exps+=formula | 'program' progvars+=ID '=' progs+=program)
     ( ','
       ( vars+=ID '=' exps+=formula | 'program' progvars+=ID '=' progs+=program)
     )*
  ( 'obtain' obtain=fact )?
  ( 'as' name=(ID | FORMULA_ID) )?
  ;

push :
  'push' form=formula 'from' id=(ID | FORMULA_ID)
  ( 'obtain' obtain=fact )?
  ( 'as' name=(ID | FORMULA_ID) )?
  ;

pop :
  'pop' 'from' id=(ID | FORMULA_ID)
  ( 'obtain' obtain=fact )?
  ( 'as' name=(ID | FORMULA_ID) )?
  ;

mp :
  'mp' first=(ID | FORMULA_ID) 'and' second=(ID | FORMULA_ID)
  ( 'obtain' obtain=fact )?
  ( 'as' name=(ID | FORMULA_ID) )?
  ;

gen :
  'gen' id=(ID | FORMULA_ID) ( 'with' program )?
  ( 'obtain' obtain=fact )?
  ( 'as' name=(ID | FORMULA_ID) )?
  ;

theorem :
  'thm' (fact | formula) 'as' name=(ID | FORMULA_ID)
  'proof'
  command*
  'end'
  ;

assume :
  'assume' formula ( 'as' name=(ID | FORMULA_ID) )?
  ;

fact :
    ( premiss+=formula
      ( ',' premiss+=formula )*
    )? '|-' concl=formula
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
  | fst=program ';' snd=program #Seq
  | program '*' #Kleene
  | fst=program '+' snd=program #Choice
  | '?' formula #Test
  | '(' program ')' #ParenProgram
  ;

ID  : [A-Za-z_][A-Za-z0-9$_]* ;
WS : [ \t\r\n]+ -> skip ;
COMMENT : '%' .*? '\n' -> skip ;
FORMULA_ID : '#' [0-9]+ ;
STRING_LIT : '"' .*? '"' ;
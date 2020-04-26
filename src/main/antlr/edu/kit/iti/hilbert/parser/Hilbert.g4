/*
 * This file is part of Dynamic David.
 *
 * (C) 2020 Mattias Ulbrich, Karlsruhe Institute of Technology
 *
 * This is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free
 * Software Foundation, either version 3 of the License, or (at your
 * option) any later version.
 *
 * DIVE is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with DIVE.  If not, see <http://www.gnu.org/licenses/>.
 *
 * @license GPL-3.0-or-later
 */

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
  // TODO Perhaps make this change lexer mode?
  | 'help' ( name=(ID|'inst'|'push'|'pop'|'mp'|'gen'|'thm'|'assume'|'set') )? # Help
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
  'assume' (formula | fact) ( 'as' name=(ID | FORMULA_ID) )?
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
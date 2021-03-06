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
package edu.kit.iti.hilbert.parser;

import org.antlr.v4.runtime.ANTLRErrorListener;
import org.antlr.v4.runtime.BailErrorStrategy;
import org.antlr.v4.runtime.BaseErrorListener;
import org.antlr.v4.runtime.DefaultErrorStrategy;
import org.antlr.v4.runtime.InputMismatchException;
import org.antlr.v4.runtime.Parser;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.misc.ParseCancellationException;

/**
 * Excerpted from "The Definitive ANTLR 4 Reference",
 * published by The Pragmatic Bookshelf.
 * Copyrights apply to this code. It may not be used to create training material,
 * courses, books, articles, and the like. Contact us if you are in doubt.
 * We make no guarantees that this code is fit for any purpose.
 * Visit http://www.pragmaticprogrammer.com/titles/tpantlr2 for more book information.
 *
 * This is a modified version from the original implementation since it also captures the
 * error message.
 *
 * @see BailErrorStrategy
 ***/
public class BailOutErrorStrategy extends DefaultErrorStrategy {


    private String lastErrorMessage = "";
    // private int lastErrorLine;
    // private int lastErrorCharInLine;

    public final ANTLRErrorListener ERROR_LISTENER = new BaseErrorListener() {
        @Override
        public void syntaxError(Recognizer<?, ?> recognizer, Object offendingSymbol,
                                int line, int charPositionInLine, String msg, RecognitionException e) {
            // BailOutErrorStrategy.this.line = line;
            // BailOutErrorStrategy.this.charInLine = charPositionInLine;
            BailOutErrorStrategy.this.lastErrorMessage = msg;
        }
    };

    /** Instead of recovering from exception e, rethrow it wrapped
     *  in a generic RuntimeException so it is not caught by the
     *  rule function catches.  Exception e is the "cause" of the
     *  RuntimeException.
     */
    @Override
    public void recover(Parser recognizer, RecognitionException e) {
        for(ParserRuleContext context = recognizer.getContext(); context != null; context = context.getParent()) {
            context.exception = e;
        }

        // thus fill #lastErrorMessage
        reportError(recognizer, e);

        throw new ParseCancellationException(lastErrorMessage, e);
    }

    /** Make sure we don't attempt to recover inline; if the parser
     *  successfully recovers, it won't throw an exception.
     */
    @Override
    public Token recoverInline(Parser recognizer) {
        InputMismatchException e = new InputMismatchException(recognizer);

        for(ParserRuleContext context = recognizer.getContext(); context != null; context = context.getParent()) {
            context.exception = e;
        }

        // thus fill #lastErrorMessage
        reportError(recognizer, e);

        throw new ParseCancellationException(lastErrorMessage, e);
    }

    /** Make sure we don't attempt to recover from problems in subrules. */
    @Override
    public void sync(Parser recognizer) { }
}


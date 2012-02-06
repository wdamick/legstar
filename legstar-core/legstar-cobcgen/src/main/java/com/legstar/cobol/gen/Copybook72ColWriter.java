package com.legstar.cobol.gen;

import java.io.IOException;
import java.io.Writer;

import org.antlr.stringtemplate.AutoIndentWriter;

/**
 * Writes statements that fit into 72 columns.
 * <p/>
 * This does not use the AutoIndentWriter indent capabilities so all indents
 * from templates are ignored.
 * 
 */
public class Copybook72ColWriter extends AutoIndentWriter {

    /** Column where statements end. */
    public static final int STATEMENTS_LAST_COLUMN = 72;

    /** New line and indentation (start at column 12, area B). */
    public static final String LINE_WRAP = "\n           ";

    /** New line and literal continuation (start at column 12, area B). */
    public static final String LINE_CONTINUE_LITERAL = "\n      -    ";

    /**
     * Alphanumeric literals are started and end with the same delimiter (either
     * quote or apost) but might contain escaped delimiters which are sequences
     * of double delimiters. THE MAYBE-CLOSED status corresponds to the case
     * where we have parsed a closing delimiter but are not sure yet if it will
     * not be followed by another delimiter which would mean the literal is not
     * closed yet.
     */
    private enum AlphanumLiteralStatus {
        NOT_STARTED, STARTED, MAYBE_CLOSED
    };

    /** Current status when alphanumeric literals are encountered. */
    private AlphanumLiteralStatus alphanumLiteralStatus = AlphanumLiteralStatus.NOT_STARTED;

    /**
     * When an alphanumeric literal is started, this will hold the delimiter
     * character that started the literal.
     */
    private char alphanumLiteralDelimiter;

    public Copybook72ColWriter(Writer out) {
        super(out);
    }

    /**
     * Almost same code as AutoIndentWriter#write(String str) but prevents code
     * from spilling beyond max column.
     * <p/>
     * Also fixes a bug in StringTemplate where the charPosition was incorrect
     * on Windows following a \r\n sequence.
     * 
     * */
    public int write(String str) throws IOException {
        int n = writeWrapSeparator(str, LINE_WRAP);
        for (int i = 0; i < str.length(); i++) {
            char c = str.charAt(i);
            // found \n or \r\n newline?
            if (c == '\r' || c == '\n') {
                atStartOfLine = true;
                charPosition = 0;
                n += newline.length();
                out.write(newline);
                // skip an extra char upon \r\n
                if ((c == '\r' && (i + 1) < str.length() && str.charAt(i + 1) == '\n')) {
                    i++; // loop iteration i++ takes care of skipping 2nd char
                }
                continue;
            }
            // normal character
            // check to see if we are at the start of a line
            if (atStartOfLine) {
                atStartOfLine = false;
            }
            // Keep track of the status for alphanumeric literals
            trackAlphanumLiteral(c);
            // if we are about to write past column 72, assume this is a value
            // literal that needs to be continued
            if (charPosition == STATEMENTS_LAST_COLUMN) {
                n += continueLiteral();
            }
            n++;
            out.write(c);
            charPosition++;

        }
        return n;
    }

    /**
     * Detect the start and close of alphanumeric literals.
     * 
     * @param c the current parsed character
     */
    protected void trackAlphanumLiteral(char c) {
        if (c == '\'' || c == '\"') {
            switch (alphanumLiteralStatus) {
            case NOT_STARTED:
                alphanumLiteralStatus = AlphanumLiteralStatus.STARTED;
                alphanumLiteralDelimiter = c;
                break;
            case STARTED:
                if (c == alphanumLiteralDelimiter) {
                    alphanumLiteralStatus = AlphanumLiteralStatus.MAYBE_CLOSED;
                }
                break;
            case MAYBE_CLOSED:
                if (c == alphanumLiteralDelimiter) {
                    alphanumLiteralStatus = AlphanumLiteralStatus.STARTED;
                } else {
                    alphanumLiteralStatus = AlphanumLiteralStatus.NOT_STARTED;
                }

            }
        } else {
            if (alphanumLiteralStatus
                    .equals(AlphanumLiteralStatus.MAYBE_CLOSED)) {
                alphanumLiteralStatus = AlphanumLiteralStatus.NOT_STARTED;
            }
        }
    }

    /**
     * Literals that are about to exceed column 72 need to be continued on the
     * next line.
     * <p/>
     * Alphanumeric literals are special because the continued line needs to
     * start with the same delimiter the alphanumeric literal is using (either
     * quote or apost).
     * 
     * @return the number of characters written
     * @throws IOException if writing fails
     */
    protected int continueLiteral() throws IOException {
        out.write(LINE_CONTINUE_LITERAL);
        charPosition = LINE_CONTINUE_LITERAL.length() - 1;
        int n = LINE_CONTINUE_LITERAL.length();
        if (alphanumLiteralStatus
                    .equals(AlphanumLiteralStatus.STARTED)) {
            out.write(alphanumLiteralDelimiter);
            charPosition++;
            n++;
        }
        return n;
    }

    /**
     * Wraps preemptively when the string to be printed would cause the line to
     * exceed the max line length.
     * <p/>
     * 
     * @param str the string to be printed
     * @param wrap the character sequence that wraps
     * @return how many characters were printed
     * @throws IOException if writing fails
     */
    protected int writeWrapSeparator(String str, String wrap)
            throws IOException {
        if (atStartOfLine) {
            return 0;
        }
        int n = 0;
        int newLinePos = str.indexOf(newline);
        int newCharPosition = charPosition
                + ((newLinePos > -1) ? newLinePos : str.length());
        if (newCharPosition > STATEMENTS_LAST_COLUMN) {
            // ok to wrap
            // Walk wrap string and look for A\nB. Spit out A\n
            // then spit out B.
            for (int i = 0; i < wrap.length(); i++) {
                char c = wrap.charAt(i);
                if (c == '\n') {
                    n++;
                    out.write(c);
                    charPosition = 0;
                    // continue writing any chars out
                } else { // write A or B part
                    n++;
                    out.write(c);
                    charPosition++;
                }
            }
        }
        return n;
    }

}

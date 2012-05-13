package com.legstar.cobol.gen;

import java.io.IOException;
import java.io.Writer;
import java.util.Arrays;

import org.antlr.stringtemplate.AutoIndentWriter;
import org.apache.commons.lang.StringUtils;

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

    /** Sentence continuation line (start at column 12, area B). */
    public static final String LINE_CONTINUE_SENTENCE = "           ";

    /** New line and literal continuation (start at column 12, area B). */
    public static final String LINE_CONTINUE_LITERAL = "      -    ";

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

    /**
     * Keeps track of the indentation so that continued sentences can have the
     * same indentation.
     */
    private int indentPos;

    /**
     * Used to count tokens when we need to determine where a COBOL data item
     * description begins.
     */
    private int tokenCounter = -1;

    /**
     * Non alphanumeric literals are all considered keywords (sequences of
     * non-space characters)
     */
    private StringBuilder keyword = new StringBuilder();

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
        trackIndentation(str);
        int n = 0;
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

            // if we are about to write past column 72, break using continuation
            // if necessary
            if (charPosition == STATEMENTS_LAST_COLUMN) {
                if (alphanumLiteralStatus == AlphanumLiteralStatus.NOT_STARTED) {
                    n += continueKeyword(indentPos);
                } else {
                    n += continueAlphaLiteral();
                }
            }
            n++;
            writeKeywordOrAlphaLiteral(c);
            charPosition++;

        }
        writeKeyword();
        return n;
    }

    /**
     * When a white space is encountered, we consider a keyword as delimited and
     * write it out. On non space characters, if we are not in the middle of an
     * alphanumeric literal, we consider the character as part of a keyword.
     * 
     * @param c the character being printed
     * @throws IOException if character cannot be printed
     */
    protected void writeKeywordOrAlphaLiteral(char c) throws IOException {
        if (c == ' ') {
            writeKeyword();
            out.write(c);
        } else {
            if (alphanumLiteralStatus == AlphanumLiteralStatus.NOT_STARTED) {
                keyword.append(c);
            } else {
                out.write(c);
            }
        }
    }

    /**
     * Print a keyword.
     * 
     * @throws IOException if writing fails
     */
    protected void writeKeyword() throws IOException {
        if (keyword.length() > 0) {
            out.write(keyword.toString().toCharArray());
            keyword = new StringBuilder();
        }
    }

    /**
     * In order to indent properly continued sentences (not continued literals),
     * we keep track of the character position of the COBOL name which follows
     * the COBOL level number. This is tied to the StringTemplate where we
     * assume this:
     * 
     * <pre>
     * $cobolDataItem.levelNumber;format="depth"$$cobolDataItem.levelNumber;format="level"$  $cobolDataItem.cobolName$
     * </pre>
     * 
     * @param str the string to be written
     */
    protected void trackIndentation(String str) {
        if (charPosition == 0) {
            tokenCounter = 3;
        }
        if (tokenCounter == 0) {
            indentPos = charPosition;
        }
        switch (tokenCounter) {
        case 3:
            if (StringUtils.isBlank(str)) {
                tokenCounter = 2;
            } else {
                tokenCounter = -1;
            }
            break;
        case 2:
            if (str.matches("\\d\\d")) {
                tokenCounter = 1;
            } else {
                tokenCounter = -1;
            }
            break;
        case 1:
            if (StringUtils.isBlank(str)) {
                tokenCounter = 0;
            } else {
                tokenCounter = -1;
            }
            break;
        default:
            tokenCounter = -1;
        }
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
    protected int continueAlphaLiteral() throws IOException {
        String continueLiteral = newline + LINE_CONTINUE_LITERAL;
        out.write(continueLiteral);
        charPosition = LINE_CONTINUE_LITERAL.length();
        int n = continueLiteral.length();
        if (alphanumLiteralStatus.equals(AlphanumLiteralStatus.STARTED)) {
            out.write(alphanumLiteralDelimiter);
            charPosition++;
            n++;
        }
        return n;
    }

    /**
     * Put a keyword on the next line (otherwise would extend past column 72).
     * 
     * @param indentPos the indentation position
     * @return the number of characters written
     * @throws IOException if writing fails
     */
    private int continueKeyword(int indentPos) throws IOException {
        return wrap("", indentPos);
    }

    /**
     * Insert the wrap sequence.
     * 
     * @param str the string to be written (this is used to reduce indent in
     *            case of leading spaces)
     * @param indentPos the indent position
     * @return the number of characters written
     * @throws IOException if writing fails
     */
    protected int wrap(String str, int indentPos) throws IOException {
        int n = 0;
        String wrap = getWrap(str, indentPos);
        // Walk wrap string and look for A\nB. Spit out A\n
        // then spit out B.
        for (int i = 0; i < wrap.length(); i++) {
            char c = wrap.charAt(i);
            if (c == '\n') {
                n++;
                out.write(newline);
                charPosition = 0;
                // continue writing any chars out
            } else { // write A or B part
                n++;
                out.write(c);
                charPosition++;
            }
        }
        return n;
    }

    /**
     * Get the wrap characters sequence including the indent for the continued
     * line.
     * <p/>
     * If the string to be written starts with spaces, we reduce the indent so
     * that the first non space character appears at the indent position.
     * 
     * @param str the string to be printed
     * @param indentPos the indent position of the continued line if line is
     *            wrapped
     * @return the wrap characters sequence including the indent for the
     *         continued line
     */
    protected String getWrap(String str, int indentPos) {
        int leadingSpaces = 0;
        for (int i = 0; i < str.length(); i++) {
            if (str.charAt(i) == ' ') {
                leadingSpaces++;
            } else {
                break;
            }
        }
        int indent = indentPos - leadingSpaces;
        if (indent < 1 || indent + str.length() > STATEMENTS_LAST_COLUMN) {
            return "\n" + LINE_CONTINUE_SENTENCE;
        }
        char[] chars = new char[indent];
        Arrays.fill(chars, ' ');
        return "\n" + new String(chars);
    }

}

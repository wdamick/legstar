/*******************************************************************************
 * Copyright (c) 2008 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.cobc.gen;

import com.legstar.codegen.CodeGenUtil;

/**
 * Represents a multi-line formatted Cobol sentence. A Cobol sentence fits into
 * the following format:
 *
 * 0 0 0 0 0 0 | 0 | 0 0 1 1 | 1               7 |
 * 1 2 3 4 5 6 | 7 | 8 9 0 1 | 2   ....        2 |
 * sequence    |   | area A  | area B            |
 * number area                
 *               ^
 *     indicator |
 *          area-|
 */
public class CobolGenSentence {

    /** First non-blank character of this sentence appears at this column
     *  (1 based). */
    private int mStartColumn;

    /** Last non-blank character of this sentence appears at this column
     *  (1 based). */
    private int mEndColumn;

    /** Current number of lines. */
    private int mLinesCount;

    /** Indicates if an inital white space is necessary before inserting
     * the next clause. */
    private boolean needSpaceSeparator;

    /** The sentence content including new lines. */
    private StringBuilder mContent;

    /** Maximum number of characters. */
    private static final int LINE_WIDTH = 72;

    /** Number of columns for sequence number area. */
    private static final int SEQ_NUM_AREA_LEN = 6;

    /** Number of columns for indicator area. */
    private static final int IND_AREA_LEN = 1;

    /** Indentation slope (number of columns increase per one additional
     *  indentation factor). */
    private static final int INDENT_SLOPE = 4;

    /** Sentence delimiter character. */
    private static final String SENTENCE_DELIM = ".";

    /** Delimiter for character strings. */
    private static final char QUOTE = '\"';

    /** Delimiter for character strings. */
    private static final char APOST = '\'';

    /**
     * Creates a cobol sentence at a specified indentation factor.
     * @param indentFactor determines if this sentence is to be indented
     * relative to area A. An indentation factor of zero means this 
     * sentence starts at column 8, an indentation factor of 1 means it starts
     * 4 characters past column 8 which is column 12 or area B. When the
     * indentation factor increases by 1 the starting position of this 
     * sentence increases by 4 columns.
     */
    public CobolGenSentence(final int indentFactor) {
        mStartColumn = SEQ_NUM_AREA_LEN + IND_AREA_LEN
        + indentFactor * INDENT_SLOPE;
        mEndColumn = mStartColumn;
        mLinesCount = 1;
        needSpaceSeparator = false;
        mContent = new StringBuilder();
        mContent.append(fillString(' ', mEndColumn));
    }

    /**
     * Adds a clause to the sentence. Adds new lines if necessary.
     * @param clause the cobol clause to be added
     */
    public final void addClause(final String clause) {
        if (clause == null || clause.length() == 0) {
            return;
        }
        String spacedClause = clause;

        /* New clause needs to be separated from the previous one */
        if (needSpaceSeparator) {
            spacedClause = ' ' + clause;
        }

        /* Tokenize the clause on space (any number) boundaries.
         * This way we reprocess each token separately. */
        String[] tokens = clause.trim().split("\\s+");
        if (tokens.length == 0) {
            return;
        } else if (tokens.length > 1) {
            addClause(tokens[0]);
            String rest = clause.substring(
                    clause.indexOf(tokens[0]) + tokens[0].length() + 1).trim();
            if (tokens[0].compareToIgnoreCase("value") == 0) {
                addValue(rest);
            } else {
                addClause(rest);
            }
            return;
        }

        /* If no place left on current line, create a new one with an
         * indent to show continuation*/
        if (mEndColumn + spacedClause.length() > LINE_WIDTH) {
            moveToNextLine(false);
            spacedClause = clause;
        }

        appendToken(spacedClause);
    }

    /**
     * Contrary to other clauses, Values might extend on multiple lines and
     * have special handling for continuation characters.
     * The rules are the following:
     * <ul>
     * <li>values can extend to column 72 (inclusive)</li>
     * <li>continued lines start with a hyphen in column 7</li>
     * <li>if the value is a numeric (no delimiters), the continuation digits
     *   show up anywhere in section B</li>
     * <li>if the value is an alphanumeric then the continued characters must
     *   first start with QUOTE or APOST depending on the delimiter
     *   in section B</li>
     * </ul>
     * @param value the COBOL value that follows the VALUE clause, including
     *  starting and ending delimiters (QUOTE or APOST)
     */
    public final void addValue(final String value) {
        if (value == null || value.length() == 0) {
            return;
        }

        /* Determine the delimiter in case this is an alphanumeric. 
         * We check the last character because values can start with
         * X" G" or N" so the last character is easier to get. The
         * other reason is that this code is called recursively so
         * the only invariant part is the end of the string value.*/
        char delimiter = value.charAt(value.length() - 1);
        if (delimiter != QUOTE && delimiter != APOST) {
            delimiter = 0;
        }

        String spacedValue = value;


        /* New clause needs to be separated from the previous one */
        if (needSpaceSeparator) {
            spacedValue = ' ' + value;
        }
        int lineCapacity = LINE_WIDTH - mEndColumn;
        if (spacedValue.length() > lineCapacity) {
            addValue(spacedValue.substring(0, lineCapacity));
            moveToNextLine(true);
            addValue(((delimiter != 0) ? delimiter : "")
                    + spacedValue.substring(lineCapacity));
        } else {
            appendToken(spacedValue);
        }
    }

    /**
     * Position on the start of the next line.
     * @param valueContinuation true if this is a subsequent value line in
     *   which case we need a special continuation character in column 7.
     */
    private void moveToNextLine(final boolean valueContinuation) {
        mContent.append(CodeGenUtil.CRLF);
        mLinesCount++;
        mEndColumn = mStartColumn + INDENT_SLOPE;
        if (valueContinuation) {
            mContent.append(fillString(' ', SEQ_NUM_AREA_LEN));
            mContent.append('-');
            mContent.append(fillString(' ', mEndColumn - SEQ_NUM_AREA_LEN - 1));
        } else {
            mContent.append(fillString(' ', mEndColumn));
        }
        needSpaceSeparator = false;
    }

    /**
     * Appends an atomic token to the content.
     * @param token the atomic token
     */
    private void appendToken(final String token) {
        mContent.append(token);
        mEndColumn += token.length();
        needSpaceSeparator = true;
    }

    /**
     * Adds the final delimiter to the sentence.
     */
    public final void close() {
        needSpaceSeparator = false;
        addClause(SENTENCE_DELIM);
    }

    /**
     * Creates a String filled with the specified character.
     * @param c character to fill
     * @param count expected string size
     * @return the new filled string
     */
    private String fillString(final char c, final int count) {
        StringBuilder sb = new StringBuilder(count);
        for (int i = 0; i < count; i++) {
            sb.append(c);
        }
        return sb.toString();
    }

    /**
     * @return the last non-blank character of this sentence appears at this
     * column (1 based)
     */
    public final int getEndColumn() {
        return mEndColumn;
    }

    /**
     * @param endColumn the last non-blank character of this sentence appears
     * at this column (1 based)
     */
    public final void setEndColumn(final int endColumn) {
        mEndColumn = endColumn;
    }

    /**
     * @return the current number of lines
     */
    public final int getLinesCount() {
        return mLinesCount;
    }

    /**
     * @param linesCount the current number of lines to set
     */
    public final void setLinesCount(final int linesCount) {
        mLinesCount = linesCount;
    }

    /**
     * @return the first non-blank character of this sentence appears at this
     * column (1 based)
     */
    public final int getStartColumn() {
        return mStartColumn;
    }

    /**
     * @param startColumn the first non-blank character of this sentence
     * appears at this column (1 based)
     */
    public final void setStartColumn(final int startColumn) {
        mStartColumn = startColumn;
    }

    /**
     * @return the sentence content including new lines
     */
    public final String toString() {
        return mContent.toString();
    }


}

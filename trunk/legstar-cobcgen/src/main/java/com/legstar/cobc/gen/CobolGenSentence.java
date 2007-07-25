package com.legstar.cobc.gen;

/**
 * Represents a multi-line formatted Cobol sentence. A Cobol sentence fits into
 * the following format:
 *
 * 0 0 0 0 0 0 | 0 | 0 0 1 1 | 1               7 |
 * 1 2 3 4 5 6 | 7 | 8 9 0 1 | 2   ....        2 |
 * sequence    |   | area A  | area B            |
 * number area                
 *               ^
 * 	   indicator |
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

	/** New line characters. */
	private static final String CRLF = "\r\n";
	
	/** Sentence delimiter character. */
	private static final String SENTENCE_DELIM = ".";
	
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
		
		/* If no place left on current line, create a new one with an
		 * indent to show continuation*/
		if (mEndColumn + spacedClause.length() > LINE_WIDTH) {
			mContent.append(CRLF);
			mLinesCount++;
			mEndColumn = mStartColumn + INDENT_SLOPE;
			mContent.append(fillString(' ', mEndColumn));
			needSpaceSeparator = false;
			spacedClause = clause;
		}
		
		mContent.append(spacedClause);
		mEndColumn += spacedClause.length();
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

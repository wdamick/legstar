package com.legstar.cobc.gen;

import com.legstar.cobc.gen.CobolGenSentence;

import junit.framework.TestCase;

public class CobolGenSentenceTest extends TestCase {
	
	public void testSingleLine() {
		CobolGenSentence s = new CobolGenSentence(0);
		assertEquals(7, s.getStartColumn());
		assertEquals(7, s.getEndColumn());
		assertEquals(1, s.getLinesCount());
		/*            1234567*/
		assertEquals("       ", s.toString());
		
		s.addClause("01");
		assertEquals(7, s.getStartColumn());
		assertEquals(9, s.getEndColumn());
		assertEquals(1, s.getLinesCount());
		/*            123456789*/
		assertEquals("       01", s.toString());

		s.addClause("VAR");
		assertEquals(7, s.getStartColumn());
		assertEquals(13, s.getEndColumn());
		assertEquals(1, s.getLinesCount());
		/*            1234567890123*/
		assertEquals("       01 VAR", s.toString());
	}

	public void test2Lines() {
		CobolGenSentence s = new CobolGenSentence(0);
		s.addClause("01");
		s.addClause("A-VERY-LONG-VARIABLE-NAME");
		s.addClause("PICTURE 9999999999V99");
		s.addClause("USAGE DISPLAY");
		assertEquals(7, s.getStartColumn());
		assertEquals(71, s.getEndColumn());
		assertEquals(1, s.getLinesCount());
		/*            000000000111111111122222222223333333333444444444455555555556666666666777*/
		/*            123456789012345678901234567890123456789012345678901234567890123456789012*/
		assertEquals("       01 A-VERY-LONG-VARIABLE-NAME PICTURE 9999999999V99 USAGE DISPLAY", s.toString());

		s.addClause("VALUE");
		s.addClause("\"23435.78\"");
		assertEquals(7, s.getStartColumn());
		assertEquals(27, s.getEndColumn());
		assertEquals(2, s.getLinesCount());
		/*            000000000111111111122222222223333333333444444444455555555556666666666777   000000000111111111122222222223333333333444444444455555555556666666666777*/
		/*            123456789012345678901234567890123456789012345678901234567890123456789012   123456789012345678901234567890123456789012345678901234567890123456789012*/
		assertEquals("       01 A-VERY-LONG-VARIABLE-NAME PICTURE 9999999999V99 USAGE DISPLAY\r\n           VALUE \"23435.78\"", s.toString());
	}

	public void testClose() {
		CobolGenSentence s = new CobolGenSentence(0);
		s.addClause("01");
		s.addClause("VAR");
		s.close();
		assertEquals(7, s.getStartColumn());
		assertEquals(14, s.getEndColumn());
		assertEquals(1, s.getLinesCount());
		/*            12345678901234*/
		assertEquals("       01 VAR.", s.toString());
	}
}

package com.legstar.cobc.gen;

import com.legstar.cobc.gen.CobolGenFormatter;
import com.legstar.coxb.impl.CArrayStringBinding;
import com.legstar.coxb.impl.CStringBinding;
import com.legstar.coxb.impl.CZonedDecimalBinding;

import junit.framework.TestCase;

public class CobolGenFormatterTest extends TestCase {
	
	public void testElementaryStringItem() {
		CStringBinding cb = new CStringBinding("astring", null, null, null, null);
		cb.setLevelNumber(5);
		cb.setCobolName("A-STRING");
		cb.setUsage("DISPLAY");
		cb.setPicture("X(8)");
		cb.setIsJustifiedRight(true);
		cb.setDefaultValue("\"BIG\"");

		/*            000000000111111111122222222223333333333444444444455555555556666666666777*/
		/*            123456789012345678901234567890123456789012345678901234567890123456789012*/
		assertEquals("           05 A-STRING PIC X(8) JUST VALUE \"BIG\".",
				CobolGenFormatter.formatCobolClause(cb, 1));
	}

	public void testElementaryZonedDecimalItem() {
		CZonedDecimalBinding cb = new CZonedDecimalBinding("astring", null, null, null, null);
		cb.setLevelNumber(10);
		cb.setCobolName("A-ZONED-DEC");
		cb.setUsage("DISPLAY");
		cb.setPicture("S9(8)V99");
		cb.setIsSignLeading(true);
		cb.setIsSignSeparate(true);

		/*            000000000111111111122222222223333333333444444444455555555556666666666777*/
		/*            123456789012345678901234567890123456789012345678901234567890123456789012*/
		assertEquals("           10 A-ZONED-DEC PIC S9(8)V99 LEADING SEPARATE.",
				CobolGenFormatter.formatCobolClause(cb, 1));
	}

	public void testFixedArrayElementaryStringItem() {
		CArrayStringBinding cb = new CArrayStringBinding("astringArray", null, null, null, null);
		cb.setLevelNumber(5);
		cb.setCobolName("A-STRING-ARRAY");
		cb.setUsage("DISPLAY");
		cb.setPicture("X(8)");
		cb.setMinOccurs(3);
		cb.setMaxOccurs(3);

		/*            000000000111111111122222222223333333333444444444455555555556666666666777*/
		/*            123456789012345678901234567890123456789012345678901234567890123456789012*/
		assertEquals("           05 A-STRING-ARRAY PIC X(8) OCCURS 3.",
				CobolGenFormatter.formatCobolClause(cb, 1));
	}

	public void testVariableArrayElementaryStringItem() {
		CArrayStringBinding cb = new CArrayStringBinding("astringArray", null, null, null, null);
		cb.setLevelNumber(5);
		cb.setCobolName("A-STRING-ARRAY");
		cb.setUsage("DISPLAY");
		cb.setPicture("X(8)");
		cb.setMinOccurs(0);
		cb.setMaxOccurs(3);
		cb.setDependingOn("ODO-COUNTER");

		/*            0000000001111111111222222222233333333334444444444555   000000000111111111122222222223333333333444444444455555555556666666666777*/
		/*            1234567890123456789012345678901234567890123456789012   123456789012345678901234567890123456789012345678901234567890123456789012*/
		assertEquals("           05 A-STRING-ARRAY PIC X(8) OCCURS 0\r\n               TO 3 DEPENDING ON ODO-COUNTER.",
				CobolGenFormatter.formatCobolClause(cb, 1));
	}

	public void testRedefinesItem() {
		CStringBinding cb = new CStringBinding("astring", null, null, null, null);
		cb.setLevelNumber(5);
		cb.setCobolName("A-STRING");
		cb.setUsage("DISPLAY");
		cb.setPicture("X(8)");
		cb.setRedefines("A-REDEFINED-STRING");

		/*            000000000111111111122222222223333333333444444444455555555556666666666777*/
		/*            123456789012345678901234567890123456789012345678901234567890123456789012*/
		assertEquals("           05 A-STRING PIC X(8) REDEFINES A-REDEFINED-STRING.",
				CobolGenFormatter.formatCobolClause(cb, 1));
	}

}

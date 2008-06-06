package com.legstar.c2ws.gen;

import junit.framework.TestCase;

public class C2wsOperationModelTest extends TestCase {
	
	public void testStructureCodeGenerationOneStructure() throws Exception {
		C2wsOperationModel c2wsOperationModel =
			new C2wsOperationModel(TestCases.getLsfileaeOperation());
		String code = c2wsOperationModel.getInputStructuresCode();
		System.out.println(code);
		assertTrue(code.contains("       05 DfhcommareaType."));
		assertTrue(code.contains("           10 COM-NUMBER PIC 9(6)."));
		assertTrue(code.contains("           10 COM-PERSONAL."));
		assertTrue(code.contains("               15 COM-NAME PIC X(20)."));
		assertTrue(code.contains("               15 COM-ADDRESS PIC X(20)."));
		assertTrue(code.contains("               15 COM-PHONE PIC X(8)."));
		assertTrue(code.contains("           10 COM-DATE PIC X(8)."));
		assertTrue(code.contains("           10 COM-AMOUNT PIC X(8)."));
		assertTrue(code.contains("           10 COM-COMMENT PIC X(9)."));
	}

	public void testStructureCodeGenerationMultipleStructures() throws Exception {
		C2wsOperationModel c2wsOperationModel =
			new C2wsOperationModel(TestCases.getLsfileacOperation());
		String code = c2wsOperationModel.getInputStructuresCode();
		System.out.println(code);
		assertTrue(code.contains("           05 QueryLimitType."));
		assertTrue(code.contains("               10 MAX-ITEMS-READ PIC 9(8) PACKED-DECIMAL."));
		assertTrue(code.contains("               10 MAX-ELAPSE-TIME PIC 9(8) PACKED-DECIMAL."));
		assertTrue(code.contains("           05 QueryDataType."));
		assertTrue(code.contains("               10 QUERY-NAME PIC X(20)."));
		assertTrue(code.contains("               10 QUERY-ADDRESS PIC X(20)."));
		assertTrue(code.contains("               10 QUERY-PHONE PIC X(8)."));
	}
}

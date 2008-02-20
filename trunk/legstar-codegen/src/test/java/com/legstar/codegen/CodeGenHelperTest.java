package com.legstar.codegen;

import junit.framework.TestCase;

public class CodeGenHelperTest extends TestCase {
	
	public void testGetQualClassName() {
		CodeGenHelper helper = new CodeGenHelper();
		assertEquals("className", helper.getQualClassName(null, "className"));
		assertEquals("package.className", helper.getQualClassName("package", "className"));
	}

}

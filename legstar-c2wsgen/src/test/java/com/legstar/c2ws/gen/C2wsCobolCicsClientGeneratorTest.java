package com.legstar.c2ws.gen;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.tools.ant.BuildException;

import junit.framework.TestCase;

public class C2wsCobolCicsClientGeneratorTest extends TestCase {
	
    /** Code will be generated here. */
    public static final File GEN_SRC_DIR = new File("src/test/gen/cobol");

	/** Logger. */
	private static final Log LOG = LogFactory.getLog(C2wsCobolCicsClientGeneratorTest.class);
	
    public void testLsfileae() {
		C2wsCobolCicsClientGenerator generator = new C2wsCobolCicsClientGenerator();
		generator.init();
		try {
			generator.execute();
			fail();
		} catch (BuildException e) {
			assertEquals("com.legstar.codegen.CodeGenMakeException: Missing cixs operation parameter", e.getMessage());
		}
		generator.setCixsOperation(TestCases.getLsfileaeOperation());
		try {
			generator.execute();
			fail();
		} catch (BuildException e) {
			assertEquals("com.legstar.codegen.CodeGenMakeException: java.lang.IllegalArgumentException: No directory name was specified", e.getMessage());
		}
		generator.setTargetCobolDir(GEN_SRC_DIR);
		try {
			generator.execute();
			fail();
		} catch (BuildException e) {
			assertEquals("com.legstar.codegen.CodeGenMakeException: You must specify a service URI", e.getMessage());
		}
		generator.setServiceURI("svn://192.168.0.2");
		try {
			generator.execute();
			fail();
		} catch (BuildException e) {
			assertEquals("com.legstar.codegen.CodeGenMakeException: URI svn://192.168.0.2 must have http scheme", e.getMessage());
		}
		generator.setServiceURI("http://192.168.0.2");
		/* Check that service name has a default */
		assertEquals("lsfileae", generator.getServiceName());
		generator.setServiceName("lsfileae");
		try {
			generator.execute();
		} catch (BuildException e) {
			fail(e.getMessage());
		}
		String resStr;
		try {
			BufferedReader in = new BufferedReader(new FileReader(GEN_SRC_DIR + "/" + generator.getCixsOperation().getCicsProgramName() + ".cbl"));
			resStr = "";
			String str = in.readLine();
			while (str != null) {
				LOG.debug(str);
				resStr += str;
				str = in.readLine();
			}
			in.close();
			assertTrue(resStr.contains("       PROGRAM-ID. LSFILEAE."));
		} catch (FileNotFoundException e) {
			fail(e.getMessage());
		} catch (IOException e) {
			fail(e.getMessage());
		}
		
	}

}

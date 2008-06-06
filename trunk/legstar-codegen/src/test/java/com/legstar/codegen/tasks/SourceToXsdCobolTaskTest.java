package com.legstar.codegen.tasks;

import java.io.File;

import org.apache.tools.ant.BuildException;

import com.legstar.codegen.CodeGenMakeException;
import com.legstar.codegen.models.SourceToXsdCobolModel;

import junit.framework.TestCase;

public class SourceToXsdCobolTaskTest extends TestCase {
	
    /**
     * Invalid target directory should be reported.
     *
     * @throws Exception Any exception encountered
     */
    public void testInvalidTargetDir() throws Exception {
    	/* No target dir at all (should use the default "schema" dir */
    	SourceToXsdCobolTaskImpl xca = new SourceToXsdCobolTaskImpl();
    	xca.setModel(new SourceToXsdCobolModelImpl());
    	try {
    		xca.checkInput(true, true);
    		fail("testInvalidTargetDir");
    	} catch (BuildException e) {
    		assertEquals("You must provide a target directory", e.getMessage());
    	}
    	
    	/* Non existant */
    	xca.setTargetDir(new File("nonexistant"));
    	try {
    		xca.checkInput(true, true);
    		fail("testInvalidTargetDir");
    	} catch (BuildException e) {
    		assertEquals("Directory nonexistant does not exist", e.getMessage());
    	}

    	/* Not a directory */
    	xca.setTargetDir(new File("src/test/resources/log4j.properties"));
    	try {
    		xca.checkInput(true, true);
    		fail("testInvalidTargetDir");
    	} catch (BuildException e) {
    		assertEquals("src\\test\\resources\\log4j.properties is not a directory or is not writable", e.getMessage());
    	}
    }

	
	private class SourceToXsdCobolTaskImpl extends SourceToXsdCobolTask {
		
	}
	
	private class SourceToXsdCobolModelImpl extends SourceToXsdCobolModel {

		public void generateBuild(File scriptFile) throws CodeGenMakeException {
			// TODO Auto-generated method stub
			
		}

	}

}

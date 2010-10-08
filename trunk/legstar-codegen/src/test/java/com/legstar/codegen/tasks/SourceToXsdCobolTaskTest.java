/*******************************************************************************
 * Copyright (c) 2010 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.codegen.tasks;

import java.io.File;
import java.util.Properties;

import org.apache.tools.ant.BuildException;

import com.legstar.codegen.CodeGenMakeException;
import com.legstar.codegen.models.SourceToXsdCobolModel;

import junit.framework.TestCase;

/**
 * Test source to xsd generic code.
 * 
 */
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
            assertEquals(
                    "src\\test\\resources\\log4j.properties is not a directory or is not writable",
                    e.getMessage());
        }
    }

    /**
     * Place holder class.
     * 
     */
    private class SourceToXsdCobolTaskImpl extends SourceToXsdCobolTask {

    }

    /**
     * Place holder class.
     * 
     */
    private class SourceToXsdCobolModelImpl extends SourceToXsdCobolModel {

        /** {@inheritDoc} */
        public void generateBuild(final File scriptFile)
                throws CodeGenMakeException {
        }

        /** {@inheritDoc} */
        @Override
        public Properties toProperties() {
            return null;
        }

    }

}

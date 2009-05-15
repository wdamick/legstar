/*******************************************************************************
 * Copyright (c) 2009 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.cobc.gen;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

import junit.framework.TestCase;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.codegen.CodeGenUtil;

/**
 * Generic helper methods for testing.
 *
 */
public abstract class AbstractTester extends TestCase {

    /** Parent generation folder. */
    public static final File GEN_DIR = new File("target/src/gen");

    /** Logger. */
    private final Log _log = LogFactory.getLog(AbstractTester.class);
    
    /** {@inheritDoc} */
    public void setUp() {
        CodeGenUtil.checkDirectory(GEN_DIR, true);
    }
    
    /**
     * A general purpose reader that gets the file content into a string.
     * @param srcDir the location of the source artifact
     * @param srcName the source artifact name
     * @return a string containing the generated source
     * @throws IOException if something goes wrong
     */
    public String getSource(
            final File srcDir, final String srcName) throws IOException {
        return getSource(new File(srcDir, srcName));
    }

    /**
     * A general purpose reader that gets the file content into a string.
     * @param fileName the name of the file pointing to source
     * @return a string containing the generated source
     * @throws IOException if something goes wrong
     */
    public String getSource(final String fileName) throws IOException {
        return getSource(new File(fileName));
    }
    /**
     * A general purpose reader that gets the file content into a string.
     * @param file the file pointing to source
     * @return a string containing the generated source
     * @throws IOException if something goes wrong
     */
    public String getSource(final File file) throws IOException {
        BufferedReader in = new BufferedReader(
                new FileReader(file));
        String resStr = "";
        String str = in.readLine();
        while (str != null) {
            _log.debug(str);
            resStr += str;
            str = in.readLine();
        }
        in.close();
        return resStr;
    }
}

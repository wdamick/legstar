/*******************************************************************************
 * Copyright (c) 2015 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.cobc.gen;

import java.io.StringWriter;

import com.legstar.cobc.AbstractTest;
import com.legstar.cobol.gen.Copybook72ColWriter;
import com.legstar.codegen.CodeGenUtil;

public class Copybook72ColWriterTest extends AbstractTest {

    private Copybook72ColWriter writer;
    StringWriter out;

    public void setUp() {
        out = new StringWriter();
        writer = new Copybook72ColWriter(out);
    }

    public void testKeywordBeyondColumn72() throws Exception {
        writer.write("       01 SOME-ARRAY-VARIABLE OCCURS 1 TO 1000 DEPENDING ON SOME-INTEGER-VARIABLE.");
        out.flush();
        assertEquals(
                "       01 SOME-ARRAY-VARIABLE OCCURS 1 TO 1000 DEPENDING ON "
                        + CodeGenUtil.CRLF
                        + "           SOME-INTEGER-VARIABLE.", out.toString());

    }

    public void testAlphanumericLiteralExtendsBeyondColumn72() throws Exception {
        writer.write("       01 SOME-ARRAY-VARIABLE PIC X(255) VALUE \" A VALUE THAT EXTENDS PAST COLUMN 72 WITH WHITE SPACES\"");
        out.flush();
        assertEquals(
                "       01 SOME-ARRAY-VARIABLE PIC X(255) VALUE \" A VALUE THAT EXTENDS PA"
                        + CodeGenUtil.CRLF
                        + "      -    \"ST COLUMN 72 WITH WHITE SPACES\"",
                out.toString());

    }

    public void testLignsWithExactly72Characters() throws Exception {
        writer.write("      * ----------------------------------------------------------------"
                + CodeGenUtil.CRLF
                + "      * Generated copybook for Person"
                + CodeGenUtil.CRLF
                + "      * ----------------------------------------------------------------");
        out.flush();
        assertEquals(
                "      * ----------------------------------------------------------------"
                        + CodeGenUtil.CRLF
                        + "      * Generated copybook for Person"
                        + CodeGenUtil.CRLF
                        + "      * ----------------------------------------------------------------",
                out.toString());

    }

    public boolean isCreateReferences() {
        return false;
    }

}

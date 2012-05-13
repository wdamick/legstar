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

    public boolean isCreateReferences() {
        return false;
    }

}

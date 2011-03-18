/*******************************************************************************
 * Copyright (c) 2011 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.cobc.gen;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

import com.legstar.coxb.impl.reflect.CComplexReflectBinding;
import com.legstar.coxb.host.HostException;

/**
 * Test the CobolGenVisitor class.
 *
 */
public class CobolGenVisitorTest extends AbstractTester {

    /** Pattern for temporary files. */
    private static final String TEMP_PATTERN = "legstar";

    /** Suffix for temporary files. */
    private static final String TEMP_SUFFIX = ".temp";

    /**
     * Test with LSFILEAE.
     * @throws Exception if test fails.
     */
    public void testLsfileae() throws Exception {

        File outFile = File.createTempFile(TEMP_PATTERN, TEMP_SUFFIX);
        processLsfileae(outFile);
        String source = getSource(outFile);
        assertTrue(source.contains("01 COM-LSFILEAE."));
        assertTrue(source.contains("02 COM-NUMBER PIC 9(6)."));
        assertTrue(source.contains("02 COM-PERSONAL."));
        assertTrue(source.contains("03 COM-NAME PIC X(20)."));
    }

    /**
     * Test with DPLARCHT.
     * @throws Exception if test fails.
     */
    public void testDplarcht() throws Exception {

        File outFile = File.createTempFile(TEMP_PATTERN, TEMP_SUFFIX);
        processDplarcht(outFile);
        String source = getSource(outFile);
        assertTrue(source.contains("01 COM-DPLARCHT."));
        assertTrue(source.contains("03 LS-MAX-ITEMS PIC 9(4) REDEFINES LS-ALL-ITEMS."));
        assertTrue(source.contains("04 LS-ITEMS-COUNT PIC 9(9) COMP-5."));
        assertTrue(source.contains("04 LS-ITEMS-ARRAY OCCURS 1 TO 500 DEPENDING ON"));
        assertTrue(source.contains("LS-ITEMS-COUNT."));
        assertTrue(source.contains("05 LS-FILES-DATA."));
        assertTrue(source.contains("05 LS-PROGRAMS-DATA REDEFINES LS-FILES-DATA."));
        assertTrue(source.contains("05 LS-TRANSACTIONS-DATA REDEFINES LS-FILES-DATA."));
    }

    /**
     * Test with MSNSearch request.
     * @throws Exception if test fails.
     */
    public void testMSNSearchRequest() throws Exception {

        File outFile = File.createTempFile(TEMP_PATTERN, TEMP_SUFFIX);
        processMSNSearchRequest(outFile);
        String source = getSource(outFile);
        assertTrue(source.contains("02 Flags--C PIC 9(9) BINARY."));
        assertTrue(source.contains("02 SourceRequest--C PIC 9(9) BINARY."));
        assertTrue(source.contains("03 Flags PIC X(32) OCCURS 1 TO 10 DEPENDING ON Flags--C."));
        assertTrue(source.contains("04 Longitude COMP-2."));
        assertTrue(source.contains("05 SortBy PIC X(32) OCCURS 1 TO 10 DEPENDING ON"));
        assertTrue(source.contains("SortBy--C."));
    }

    /**
     * Test with MSNSearch response.
     * @throws Exception if test fails.
     */
    public void testMSNSearchResponse() throws Exception {

        File outFile = File.createTempFile(TEMP_PATTERN, TEMP_SUFFIX);
        processMSNSearchResponse(outFile);
        String source = getSource(outFile);
        assertTrue(source.contains("05 COM-MSNSEARCH-RESPONSE."));
        assertTrue(source.contains("25 Results."));
        assertTrue(source.contains("30 Result OCCURS 0 TO 10 DEPENDING ON"));
        assertTrue(source.contains("Result--C."));
        assertTrue(source.contains("35 SearchTags PIC X(32)."));
        assertTrue(source.contains("40 Minute PIC 9(9) COMP-5."));
        assertTrue(source.contains("40 Latitude COMP-2."));
        assertTrue(source.contains("40 ImageHeight PIC 9(9) COMP-5."));
    }

    /**
     * Bind to a JAXB object for LSFILEAE and visit with CobolGenVisitor.
     * @param outFile output file for generated COBOL code
     * @throws HostException if binding fails
     * @throws IOException if writing fails
     */
    private void processLsfileae(final File outFile) throws HostException, IOException {
        com.legstar.test.coxb.lsfileae.ObjectFactory of = new com.legstar.test.coxb.lsfileae.ObjectFactory();
        com.legstar.test.coxb.lsfileae.Dfhcommarea dfhcommarea = of.createDfhcommarea();
        CComplexReflectBinding ccem = new CComplexReflectBinding(of, dfhcommarea);
        ccem.setCobolName("COM-LSFILEAE");
        BufferedWriter writer = new BufferedWriter(new FileWriter(outFile));
        CobolGenVisitor cev = new CobolGenVisitor(writer);
        ccem.accept(cev);
        writer.close();
    }

    /**
     * Bind to a JAXB object for DPLARCHT and visit with CobolGenVisitor.
     * @param outFile output file for generated COBOL code
     * @throws HostException if binding fails
     * @throws IOException if writing fails
     */
    private void processDplarcht(final File outFile) throws HostException, IOException {
        com.legstar.test.coxb.dplarcht.ObjectFactory of = new com.legstar.test.coxb.dplarcht.ObjectFactory();
        com.legstar.test.coxb.dplarcht.Dfhcommarea dfhcommarea = of.createDfhcommarea();
        CComplexReflectBinding ccem = new CComplexReflectBinding(of, dfhcommarea);
        ccem.setCobolName("COM-DPLARCHT");
        BufferedWriter writer = new BufferedWriter(new FileWriter(outFile));
        CobolGenVisitor cev = new CobolGenVisitor(writer);
        ccem.accept(cev);
        writer.close();
    }

    /**
     * Bind to a JAXB object for MSNSearch request and visit with CobolGenVisitor.
     * @param outFile output file for generated COBOL code
     * @throws HostException if binding fails
     * @throws IOException if writing fails
     */
    private void processMSNSearchRequest(final File outFile) throws HostException, IOException {
        com.legstar.test.coxb.MSNSearch.ObjectFactory of =
            new com.legstar.test.coxb.MSNSearch.ObjectFactory();
        com.legstar.test.coxb.MSNSearch.Search search = of.createSearch();
        CComplexReflectBinding ccem = new CComplexReflectBinding(
                of, search);
        ccem.setCobolName("COM-MSNSEARCH");
        BufferedWriter writer = new BufferedWriter(new FileWriter(outFile));
        CobolGenVisitor cev = new CobolGenVisitor(writer);
        ccem.accept(cev);
        writer.close();
    }

    /**
     * Bind to a JAXB object for MSNSearch request and visit with CobolGenVisitor.
     * @param outFile output file for generated COBOL code
     * @throws HostException if binding fails
     * @throws IOException if writing fails
     */
    private void processMSNSearchResponse(final File outFile) throws HostException, IOException {
        com.legstar.test.coxb.MSNSearch.ObjectFactory of =
            new com.legstar.test.coxb.MSNSearch.ObjectFactory();
        com.legstar.test.coxb.MSNSearch.SearchResponse searchResponse = of.createSearchResponse();
        CComplexReflectBinding ccem = new CComplexReflectBinding(
                of, searchResponse);
        ccem.setCobolName("COM-MSNSEARCH-RESPONSE");
        BufferedWriter writer = new BufferedWriter(new FileWriter(outFile));
        CobolGenVisitor cev = new CobolGenVisitor(3, 5, writer);
        ccem.accept(cev);
        writer.close();
    }

}

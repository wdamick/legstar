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

import java.io.IOException;

import com.legstar.cobc.AbstractTest;
import com.legstar.cobol.model.CobolDataItem;
import com.legstar.coxb.host.HostException;
import com.legstar.coxb.impl.reflect.CComplexReflectBinding;

/**
 * Test the CobolGenVisitor class.
 * 
 */
public class CobolGenVisitorTest extends AbstractTest {

    /** True when references should be created. */
    private static final boolean CREATE_REFERENCES = false;

    public boolean isCreateReferences() {
        return CREATE_REFERENCES;
    }

    /**
     * Test with LSFILEAE.
     * 
     * @throws Exception if test fails.
     */
    public void testLsfileae() throws Exception {
        CobolDataItem cobolDataItem = processLsfileae();
        check(cobolDataItem.toString());
    }

    /**
     * Test with DPLARCHT.
     * 
     * @throws Exception if test fails.
     */
    public void testDplarcht() throws Exception {
        CobolDataItem cobolDataItem = processDplarcht();
        check(cobolDataItem.toString());
    }

    /**
     * Test with MSNSearch request.
     * 
     * @throws Exception if test fails.
     */
    public void testMSNSearchRequest() throws Exception {
        CobolDataItem cobolDataItem = processMSNSearchRequest();
        check(cobolDataItem.toString());
    }

    /**
     * Test with MSNSearch response.
     * 
     * @throws Exception if test fails.
     */
    public void testMSNSearchResponse() throws Exception {
        CobolDataItem cobolDataItem = processMSNSearchResponse();
        check(cobolDataItem.toString());
    }

    /**
     * Bind to a JAXB object for LSFILEAE and visit with CobolGenVisitor.
     * 
     * @return the generated COBOL data item
     * @throws HostException if binding fails
     * @throws IOException if writing fails
     */
    private CobolDataItem processLsfileae() throws HostException, IOException {
        com.legstar.test.coxb.lsfileae.ObjectFactory of = new com.legstar.test.coxb.lsfileae.ObjectFactory();
        com.legstar.test.coxb.lsfileae.Dfhcommarea dfhcommarea = of
                .createDfhcommarea();
        CComplexReflectBinding ccem = new CComplexReflectBinding(of,
                dfhcommarea);
        ccem.setCobolName("COM-LSFILEAE");
        return gen(ccem);
    }

    /**
     * Bind to a JAXB object for DPLARCHT and visit with CobolGenVisitor.
     * 
     * @return the generated COBOL data item
     * @throws HostException if binding fails
     * @throws IOException if writing fails
     */
    private CobolDataItem processDplarcht() throws HostException, IOException {
        com.legstar.test.coxb.dplarcht.ObjectFactory of = new com.legstar.test.coxb.dplarcht.ObjectFactory();
        com.legstar.test.coxb.dplarcht.Dfhcommarea dfhcommarea = of
                .createDfhcommarea();
        CComplexReflectBinding ccem = new CComplexReflectBinding(of,
                dfhcommarea);
        ccem.setCobolName("COM-DPLARCHT");
        return gen(ccem);
    }

    /**
     * Bind to a JAXB object for MSNSearch request and visit with
     * CobolGenVisitor.
     * 
     * @return the generated COBOL data item
     * @throws HostException if binding fails
     * @throws IOException if writing fails
     */
    private CobolDataItem processMSNSearchRequest() throws HostException,
            IOException {
        com.legstar.test.coxb.MSNSearch.ObjectFactory of = new com.legstar.test.coxb.MSNSearch.ObjectFactory();
        com.legstar.test.coxb.MSNSearch.Search search = of.createSearch();
        CComplexReflectBinding ccem = new CComplexReflectBinding(of, search);
        ccem.setCobolName("COM-MSNSEARCH");
        return gen(ccem);
    }

    /**
     * Bind to a JAXB object for MSNSearch response and visit with
     * CobolGenVisitor.
     * 
     * @return the generated COBOL data item
     * @throws HostException if binding fails
     * @throws IOException if writing fails
     */
    private CobolDataItem processMSNSearchResponse() throws HostException,
            IOException {
        com.legstar.test.coxb.MSNSearch.ObjectFactory of = new com.legstar.test.coxb.MSNSearch.ObjectFactory();
        com.legstar.test.coxb.MSNSearch.SearchResponse searchResponse = of
                .createSearchResponse();
        CComplexReflectBinding ccem = new CComplexReflectBinding(of,
                searchResponse);
        ccem.setCobolName("COM-MSNSEARCH-RESPONSE");
        return gen(ccem);
    }

    /**
     * Transform an annotation system to a COBOL model.
     * 
     * @param ccem the COBOL/JAXB binding
     * @return a COBOL root data item
     * @throws HostException if transformation fails
     */
    private CobolDataItem gen(CComplexReflectBinding ccem) throws HostException {
        CobolGenVisitor cev = new CobolGenVisitor(3, 5);
        ccem.accept(cev);
        return cev.getRootDataItem();
    }

}

/*******************************************************************************
 *  LegStar legacy Web-enablement .
 *  Copyright (C) 2007 LegSem
 *  
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *  
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *   
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 *  02110-1301  USA
 *  
 *******************************************************************************/
package com.legstar.test.coxb;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.convert.simple.CobolSimpleConverters;
import com.legstar.coxb.impl.reflect.CComplexReflectBinding;
import com.legstar.coxb.impl.visitor.CobolMarshalVisitor;
import com.legstar.coxb.impl.visitor.CobolUnmarshalVisitor;
import com.legstar.coxb.host.HostException;
import com.legstar.coxb.host.HostData;

import com.legstar.test.coxb.dplarcht.ObjectFactory;
import com.legstar.test.coxb.dplarcht.Dfhcommarea;
import com.legstar.test.coxb.dplarcht.bind.DfhcommareaBinding;

import junit.framework.TestCase;

/**
 * Test strategy choices to resolve complex redefines. 
 *
 */
public class ChoiceStrategyTest extends TestCase {
    
    /**
     * A typical content for DPLARCHT commarea.
     *              1 
     *         *                
     *                  1 
     *             0
     *                1
     *         P R O G R A M 1
     *         C                       
     *         A S M                       
     *         1750
     *                1 */
    public static final String DPLARCHT_BYTES = "0001"
        + "5c404040"
        + "c340404040404040"
        + "000000001f"
        + "0000"
        + "00000001"
        + "d7d9d6c7d9c1d4f1"
        + "c34040404040404040404040"
        + "c1e2d4404040404040404040"
        + "000006d6"
        + "00000001"
        + "404040404040404040404040404040404040404040404040"; 

    /**
     * This test fakes a situation where marshalling will use a user written
     * choice selector in order to determine which alternative to use.
     * @throws HostException if test fails
     */
    public void testDplarchtMarshalStatic() throws HostException {

        byte[] hostBytes = new byte[89];

        // Select a conversion strategy 
        CobolSimpleConverters cc = new CobolSimpleConverters(new CobolContext());
        CobolMarshalVisitor mv = new CobolMarshalVisitor(hostBytes, 0, cc);

        // Create an instance of the JAXB object factory
        ObjectFactory objectFactory = new ObjectFactory();
        // Create and populate an instance of an object (JAXB annotated)
        Dfhcommarea dfhcommarea = objectFactory.createDfhcommarea();

        com.legstar.test.coxb.dplarcht.LsRequest lsRequest = objectFactory.createLsRequest();
        dfhcommarea.setLsRequest(lsRequest);
        lsRequest.setLsRequestType(1); // request programs
        lsRequest.setLsAllItems("*");  // no limit to number of items
        com.legstar.test.coxb.dplarcht.LsSearchCriteria lsSearchCriteria = objectFactory.createLsSearchCriteria();
        lsSearchCriteria.setLsStartwith("C");
        lsSearchCriteria.setLsStartwithLen(1);
        lsRequest.setLsSearchCriteria(lsSearchCriteria);

        com.legstar.test.coxb.dplarcht.LsReply lsReplyType = objectFactory.createLsReply();
        dfhcommarea.setLsReply(lsReplyType);
        com.legstar.test.coxb.dplarcht.LsReplyData lsReplyData = objectFactory.createLsReplyData();
        lsReplyType.setLsReplyData(lsReplyData);

        lsReplyData.setLsItemsCount(1); 
        com.legstar.test.coxb.dplarcht.LsItemsArray ia = objectFactory.createLsItemsArray();

        com.legstar.test.coxb.dplarcht.LsProgramsData pt = objectFactory.createLsProgramsData();
        pt.setLsProgramName("PROGRAM1");
        pt.setLsProgramLength(1750);
        pt.setLsProgramType("C");
        pt.setLsProgramLanguage("ASM");
        pt.setLsProgramUsecount(1);

        ia.setLsProgramsData(pt);
        lsReplyData.getLsItemsArray().add(ia);

        // Traverse the object structure, visiting each node with the visitor
        DfhcommareaBinding ccem = new DfhcommareaBinding(dfhcommarea);

        mv.setOffset(0);
        ccem.accept(mv);

        assertEquals(DPLARCHT_BYTES, HostData.toHexString(hostBytes));
    }

    /**
     * Unmarshal Dplarcht.
     * @throws HostException if unmarshaling fails
     */
    public void testDplarchtUnmarshalStatic() throws HostException {

        // Create a cobol context 
        CobolContext cobolContext = new CobolContext();
        // Select a conversion strategy 
        CobolSimpleConverters cc = new CobolSimpleConverters(cobolContext);
        String hexString   = DPLARCHT_BYTES;
        byte[] hostBytes = HostData.toByteArray(hexString);

        CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0, cc);

        // Traverse the object structure, visiting each node with the visitor
        DfhcommareaBinding ccem = new DfhcommareaBinding();
        ccem.accept(uv);
        Dfhcommarea dfhcommarea = ccem.getDfhcommarea();
        checkDplarcht(dfhcommarea);
    }

    /**
     *  This test fakes a situation where marshalling will use a user written
     *  choice selector in order to determine which alternative to use.
     * @throws HostException if test fails
     *   */
    public void testDplarchtMarshalDynamic() throws HostException {

        byte[] hostBytes = new byte[89];

        // Select a conversion strategy 
        CobolSimpleConverters cc = new CobolSimpleConverters(new CobolContext());
        CobolMarshalVisitor mv = new CobolMarshalVisitor(hostBytes, 0, cc);

        // Create an instance of the JAXB object factory
        ObjectFactory objectFactory = new ObjectFactory();
        // Create and populate an instance of an object (JAXB annotated)
        Dfhcommarea dfhcommarea = objectFactory.createDfhcommarea();

        com.legstar.test.coxb.dplarcht.LsRequest lsRequest = objectFactory.createLsRequest();
        dfhcommarea.setLsRequest(lsRequest);
        lsRequest.setLsRequestType(1); // request programs
        lsRequest.setLsAllItems("*");  // no limit to number of items
        com.legstar.test.coxb.dplarcht.LsSearchCriteria lsSearchCriteria = objectFactory.createLsSearchCriteria();
        lsSearchCriteria.setLsStartwith("C");
        lsSearchCriteria.setLsStartwithLen(1);
        lsRequest.setLsSearchCriteria(lsSearchCriteria);

        com.legstar.test.coxb.dplarcht.LsReply lsReply = objectFactory.createLsReply();
        dfhcommarea.setLsReply(lsReply);
        com.legstar.test.coxb.dplarcht.LsReplyData lsReplyData = objectFactory.createLsReplyData();
        lsReply.setLsReplyData(lsReplyData);

        lsReplyData.setLsItemsCount(1); 
        com.legstar.test.coxb.dplarcht.LsItemsArray ia = objectFactory.createLsItemsArray();

        com.legstar.test.coxb.dplarcht.LsProgramsData pt = objectFactory.createLsProgramsData();
        pt.setLsProgramName("PROGRAM1");
        pt.setLsProgramLength(1750);
        pt.setLsProgramType("C");
        pt.setLsProgramLanguage("ASM");
        pt.setLsProgramUsecount(1);

        ia.setLsProgramsData(pt);
        lsReplyData.getLsItemsArray().add(ia);

        // Traverse the object structure, visiting each node with the visitor
        CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, dfhcommarea);

        mv.setOffset(0);
        ccem.accept(mv);

        assertEquals(DPLARCHT_BYTES, HostData.toHexString(hostBytes));
    }

    /**
     * Unmarshal using dynamic binding (reflexion on JAXB classes).
     * @throws HostException if unmarshal fails
     */
    public void testDplarchtUnmarshalDynamic() throws HostException {

        // Create a cobol context 
        CobolContext cobolContext = new CobolContext();
        // Select a conversion strategy 
        CobolSimpleConverters cc = new CobolSimpleConverters(cobolContext);
        String hexString   = DPLARCHT_BYTES;
        byte[] hostBytes = HostData.toByteArray(hexString);

        CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0, cc);

        // Create an instance of the JAXB object factory
        ObjectFactory objectFactory = new ObjectFactory();
        // Create an initial empty instance of an object
        Dfhcommarea dfhcommarea = objectFactory.createDfhcommarea();

        // Traverse the object structure, visiting each node with the visitor
        CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, dfhcommarea);
        ccem.accept(uv);
        checkDplarcht(dfhcommarea);
    }
    
    /**
     * Check values from Dplarcht data object.
     * @param dfhcommarea the Dplarcht data object
     */
    private void checkDplarcht(final Dfhcommarea dfhcommarea) {
        assertEquals(1, dfhcommarea.getLsReply().getLsReplyData().getLsItemsCount());
        assertEquals("ASM", dfhcommarea.getLsReply().getLsReplyData().getLsItemsArray()
                .get(0).getLsProgramsData().getLsProgramLanguage());
        assertEquals(1750, dfhcommarea.getLsReply().getLsReplyData().getLsItemsArray()
                .get(0).getLsProgramsData().getLsProgramLength());
        assertEquals("PROGRAM1", dfhcommarea.getLsReply().getLsReplyData().getLsItemsArray()
                .get(0).getLsProgramsData().getLsProgramName());
        assertEquals("C", dfhcommarea.getLsReply().getLsReplyData().getLsItemsArray()
                .get(0).getLsProgramsData().getLsProgramType());
        assertEquals(1, dfhcommarea.getLsReply().getLsReplyData().getLsItemsArray()
                .get(0).getLsProgramsData().getLsProgramUsecount());
        assertEquals("", dfhcommarea.getLsReply().getLsReplyData().getLsItemsArray()
                .get(0).getLsProgramsData().getFiller113());
        assertEquals(null, dfhcommarea.getLsReply().getLsReplyData().getLsItemsArray()
                .get(0).getLsFilesData());
        assertEquals(null, dfhcommarea.getLsReply().getLsReplyData().getLsItemsArray()
                .get(0).getLsTransactionsData());
    }
}

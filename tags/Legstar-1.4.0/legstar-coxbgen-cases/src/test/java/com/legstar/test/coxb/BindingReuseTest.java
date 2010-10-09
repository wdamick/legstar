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
package com.legstar.test.coxb;

import com.legstar.coxb.CobolBindingVisitorsFactory;
import com.legstar.coxb.CobolElementVisitor;
import com.legstar.coxb.ICobolBindingVisitorsFactory;
import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.coxb.convert.CobolConvertersFactory;
import com.legstar.coxb.convert.ICobolConverters;
import com.legstar.coxb.convert.ICobolConvertersFactory;
import com.legstar.coxb.host.HostData;
import com.legstar.coxb.host.HostException;

import junit.framework.TestCase;

/**
 * The purpose of this test case is to make sure a binding is reusable.
 * From a performance standpoint, its good to be able to reuse a binding because
 * there might be a lot of processing involved in building the children list at
 * construction time.
 *
 */
public class BindingReuseTest extends TestCase {
    
    /** The current set of COBOL converters.*/
    private ICobolConverters mCobolConverters;

    /** The binding factory. */
    private ICobolBindingVisitorsFactory mFactory;
    
    /** {@inheritDoc} */
    public void setUp() {
        ICobolConvertersFactory factory =
            CobolConvertersFactory.createCobolConvertersFactory();
        mCobolConverters = factory.createCobolConverters();
        mFactory = CobolBindingVisitorsFactory.createCobolBindingVisitorsFactory();
    }
    
    /**
     * Use the same binding to unmarshal one choice and then the alternative.
     */
    public void testChoice() {
        try {
            com.legstar.test.coxb.redsimpt.bind.DfhcommareaBinding binding =
                new com.legstar.test.coxb.redsimpt.bind.DfhcommareaBinding();
            
            com.legstar.test.coxb.redsimpt.Dfhcommarea valueObject =
                (com.legstar.test.coxb.redsimpt.Dfhcommarea) unmarshal(
                    binding, RedsimptCases.getHostBytesHex(),
                    com.legstar.test.coxb.redsimpt.Dfhcommarea.class);
            assertEquals("ABCDEFGHIJKLMNO", valueObject.getCDefinition1());
            assertTrue(null == valueObject.getCDefinition2());
            
            valueObject = (com.legstar.test.coxb.redsimpt.Dfhcommarea) unmarshal(
                    binding, RedsimptCases.getHostBytesHexSecondChoice(),
                    com.legstar.test.coxb.redsimpt.Dfhcommarea.class);
            assertTrue(null == valueObject.getCDefinition1());
            assertEquals("123456789012345", valueObject.getCDefinition2().toString());
            
            
        } catch (HostException e) {
            fail(e.getMessage());
        }
    }
    
    /**
     * Use the same binding to unmarshal a variable size array more than once with a
     * different size each time.
     */
    public void testVariableSizeArray() {
        try {
            com.legstar.test.coxb.vararcom.bind.DfhcommareaBinding binding =
                new com.legstar.test.coxb.vararcom.bind.DfhcommareaBinding();
            
            com.legstar.test.coxb.vararcom.Dfhcommarea valueObject =
                (com.legstar.test.coxb.vararcom.Dfhcommarea) unmarshal(
                    binding, VararcomCases.getHostBytesHexSome(),
                    com.legstar.test.coxb.vararcom.Dfhcommarea.class);
            assertEquals(10, valueObject.getCItemsNumber());
            assertEquals(10, valueObject.getCArray().size());
            
            valueObject = (com.legstar.test.coxb.vararcom.Dfhcommarea) unmarshal(
                    binding, VararcomCases.getHostBytesHexEmpty(),
                    com.legstar.test.coxb.vararcom.Dfhcommarea.class);
            assertEquals(0, valueObject.getCItemsNumber());
            assertEquals(0, valueObject.getCArray().size());
            
            valueObject = (com.legstar.test.coxb.vararcom.Dfhcommarea) unmarshal(
                    binding, VararcomCases.getHostBytesHexFull(),
                    com.legstar.test.coxb.vararcom.Dfhcommarea.class);
            assertEquals(250, valueObject.getCItemsNumber());
            assertEquals(250, valueObject.getCArray().size());
           
        } catch (HostException e) {
            fail(e.getMessage());
        }
    }
    
    /**
     * This case has both choices and variable size arrays.
     */
    public void testMixedTypes() {
        try {
            com.legstar.test.coxb.dplarcht.bind.DfhcommareaBinding binding =
                new com.legstar.test.coxb.dplarcht.bind.DfhcommareaBinding();
            
            com.legstar.test.coxb.dplarcht.Dfhcommarea valueObject =
                (com.legstar.test.coxb.dplarcht.Dfhcommarea) unmarshal(
                    binding, DplarchtCases.getHostBytesHexFiles(10),
                    com.legstar.test.coxb.dplarcht.Dfhcommarea.class);
            DplarchtCases.checkJavaObjectFiles(10, valueObject);
            
            valueObject = (com.legstar.test.coxb.dplarcht.Dfhcommarea) unmarshal(
                    binding, DplarchtCases.getHostBytesHex1Program(),
                    com.legstar.test.coxb.dplarcht.Dfhcommarea.class);
            DplarchtCases.checkJavaObject1Program(valueObject);
            
            valueObject = (com.legstar.test.coxb.dplarcht.Dfhcommarea) unmarshal(
                    binding, DplarchtCases.getHostBytesHexFiles(5),
                    com.legstar.test.coxb.dplarcht.Dfhcommarea.class);
            DplarchtCases.checkJavaObjectFiles(5, valueObject);

        } catch (HostException e) {
            fail(e.getMessage());
        }
    }
    
    /**
     * Convenience method to unmashal host data into a java value object.
     * @param binding the COBOL binding to use
     * @param hostDataHex the host data as hex string
     * @param clazz the value object class
     * @return an instance of the value object
     * @throws HostException if unmarshaling fails
     */
    private Object unmarshal(
            final ICobolComplexBinding binding,
            final String hostDataHex,
            final Class < ? > clazz) throws HostException {
        byte[] hostData = HostData.toByteArray(hostDataHex);
        CobolElementVisitor unmarshaler = mFactory.createUnmarshalVisitor(
                hostData, 0, getCobolConverters());
        binding.accept(unmarshaler);
        return binding.getObjectValue(clazz);
    }

    /**
     * This method returns the current set of COBOL converters.
     * @return a set of COBOL converters
     */
    public ICobolConverters getCobolConverters() {
        return mCobolConverters;
    }
}

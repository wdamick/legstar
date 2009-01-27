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
package com.legstar.messaging.impl;

import java.util.HashMap;
import java.util.Map;

import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.test.coxb.lsfileae.Dfhcommarea;
import com.legstar.test.coxb.lsfileae.bind.DfhcommareaBinding;

import junit.framework.TestCase;

public class LegStarMessageImplTest extends TestCase {
	
	public void testAddMessagePart() throws Exception {
		LegStarMessageImpl legstarMessage = new LegStarMessageImpl();
	    /* The request java object tree */
	    Dfhcommarea jaxbObject	= CobolTransformerTest.getJaxbObject();
	    DfhcommareaBinding binding = new DfhcommareaBinding(jaxbObject);
	    legstarMessage.addMessagePart(binding, 0, "IBM01147", null);
	    assertEquals(1, legstarMessage.getHeaderPart().getDataPartsNumber());
	    assertEquals(8, legstarMessage.getHeaderPart().getPayloadSize());
	    assertEquals(79, legstarMessage.getDataParts().get(0).getPayloadSize());
	    assertEquals("LSOKCOMMAREA", legstarMessage.getDataParts().get(0).getPartID());
	}

	public void testAddMessagePartExtraSize() throws Exception {
		LegStarMessageImpl legstarMessage = new LegStarMessageImpl();
	    /* The request java object tree */
	    Dfhcommarea jaxbObject	= CobolTransformerTest.getJaxbObject();
	    DfhcommareaBinding binding = new DfhcommareaBinding(jaxbObject);
	    legstarMessage.addMessagePart(binding, 94, "IBM01147", null);
	    assertEquals(1, legstarMessage.getHeaderPart().getDataPartsNumber());
	    assertEquals(8, legstarMessage.getHeaderPart().getPayloadSize());
	    assertEquals(79, legstarMessage.getDataParts().get(0).getPayloadSize());
	    assertEquals("LSOKCOMMAREA", legstarMessage.getDataParts().get(0).getPartID());
	}

	public void getBindingFromPart() throws Exception {
		LegStarMessageImpl legstarMessage = new LegStarMessageImpl();
	    /* The request java object tree */
	    Dfhcommarea jaxbObject	= CobolTransformerTest.getJaxbObject();
	    DfhcommareaBinding binding = new DfhcommareaBinding(jaxbObject);
	    legstarMessage.addMessagePart(binding, 0, "IBM01147", null);
	    
	    DfhcommareaBinding binding2 = new DfhcommareaBinding();
	    legstarMessage.getBindingFromPart(binding2, "IBM01147", null);
	    
	    assertEquals(binding.getDfhcommarea().getComNumber(),
	    		binding2.getDfhcommarea().getComNumber());
	    assertEquals(binding.getDfhcommarea().getComAmount(),
	    		binding2.getDfhcommarea().getComAmount());
	    assertEquals(binding.getDfhcommarea().getComComment(),
	    		binding2.getDfhcommarea().getComComment());
	    assertEquals(binding.getDfhcommarea().getComDate(),
	    		binding2.getDfhcommarea().getComDate());
	    assertEquals(binding.getDfhcommarea().getComPersonal().getComAddress(),
	    		binding2.getDfhcommarea().getComPersonal().getComAddress());
	    assertEquals(binding.getDfhcommarea().getComPersonal().getComName(),
	    		binding2.getDfhcommarea().getComPersonal().getComName());
	    assertEquals(binding.getDfhcommarea().getComPersonal().getComPhone(),
	    		binding2.getDfhcommarea().getComPersonal().getComPhone());
	}

	public void testContainers() throws Exception {
		LegStarMessageImpl legstarMessage = new LegStarMessageImpl();
	    /* The request java object tree */
	    Dfhcommarea jaxbObject	= CobolTransformerTest.getJaxbObject();
	    DfhcommareaBinding binding = new DfhcommareaBinding(jaxbObject);
	    legstarMessage.addMessagePart(binding, 0, "IBM01147", "CONTAINER-1");
	    Dfhcommarea jaxbObject2	= CobolTransformerTest.getJaxbObject();
	    DfhcommareaBinding binding2 = new DfhcommareaBinding(jaxbObject2);
	    legstarMessage.addMessagePart(binding2, 0, "IBM01147", "CONTAINER-2");
	    assertEquals(2, legstarMessage.getHeaderPart().getDataPartsNumber());
	    
	    Map < String, ICobolComplexBinding > bindingsMap =
	    	new HashMap < String, ICobolComplexBinding >();
	    DfhcommareaBinding binding3 = new DfhcommareaBinding();
	    DfhcommareaBinding binding4 = new DfhcommareaBinding();
	    DfhcommareaBinding binding5 = new DfhcommareaBinding();
	    bindingsMap.put("CONTAINER-1", binding3);
	    bindingsMap.put("CONTAINER-2", binding4);
	    bindingsMap.put("CONTAINER-3", binding5);
	    
	    legstarMessage.getBindingsFromParts(bindingsMap, "IBM01147");
	    
	    DfhcommareaBinding binding6 = (DfhcommareaBinding) bindingsMap.get("CONTAINER-1");
	    assertEquals(100, binding6.getDfhcommarea().getComNumber());
	    DfhcommareaBinding binding7 = (DfhcommareaBinding) bindingsMap.get("CONTAINER-2");
	    assertEquals(100, binding7.getDfhcommarea().getComNumber());
	    DfhcommareaBinding binding8 = (DfhcommareaBinding) bindingsMap.get("CONTAINER-3");
	    assertEquals(null, binding8.getDfhcommarea());
	}
}

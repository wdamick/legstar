/*******************************************************************************
 * Copyright (c) 2008 LegSem.
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
import com.legstar.test.coxb.lsfileae.DfhcommareaType;
import com.legstar.test.coxb.lsfileae.bind.DfhcommareaTypeBinding;

import junit.framework.TestCase;

public class LegStarMessageImplTest extends TestCase {
	
	public void testAddMessagePart() throws Exception {
		LegStarMessageImpl legstarMessage = new LegStarMessageImpl();
	    /* The request java object tree */
	    DfhcommareaType jaxbObject	= CobolTransformerTest.getJaxbObject();
	    DfhcommareaTypeBinding binding = new DfhcommareaTypeBinding(jaxbObject);
	    legstarMessage.addMessagePart(binding, 0, "IBM01147", null);
	    assertEquals(1, legstarMessage.getHeaderPart().getDataPartsNumber());
	    assertEquals(8, legstarMessage.getHeaderPart().getPayloadSize());
	    assertEquals(79, legstarMessage.getDataParts().get(0).getPayloadSize());
	    assertEquals("LSOKCOMMAREA", legstarMessage.getDataParts().get(0).getID());
	}

	public void testAddMessagePartExtraSize() throws Exception {
		LegStarMessageImpl legstarMessage = new LegStarMessageImpl();
	    /* The request java object tree */
	    DfhcommareaType jaxbObject	= CobolTransformerTest.getJaxbObject();
	    DfhcommareaTypeBinding binding = new DfhcommareaTypeBinding(jaxbObject);
	    legstarMessage.addMessagePart(binding, 94, "IBM01147", null);
	    assertEquals(1, legstarMessage.getHeaderPart().getDataPartsNumber());
	    assertEquals(8, legstarMessage.getHeaderPart().getPayloadSize());
	    assertEquals(79, legstarMessage.getDataParts().get(0).getPayloadSize());
	    assertEquals("LSOKCOMMAREA", legstarMessage.getDataParts().get(0).getID());
	}

	public void getBindingFromPart() throws Exception {
		LegStarMessageImpl legstarMessage = new LegStarMessageImpl();
	    /* The request java object tree */
	    DfhcommareaType jaxbObject	= CobolTransformerTest.getJaxbObject();
	    DfhcommareaTypeBinding binding = new DfhcommareaTypeBinding(jaxbObject);
	    legstarMessage.addMessagePart(binding, 0, "IBM01147", null);
	    
	    DfhcommareaTypeBinding binding2 = new DfhcommareaTypeBinding();
	    legstarMessage.getBindingFromPart(binding2, "IBM01147", null);
	    
	    assertEquals(binding.getDfhcommareaType().getComNumber(),
	    		binding2.getDfhcommareaType().getComNumber());
	    assertEquals(binding.getDfhcommareaType().getComAmount(),
	    		binding2.getDfhcommareaType().getComAmount());
	    assertEquals(binding.getDfhcommareaType().getComComment(),
	    		binding2.getDfhcommareaType().getComComment());
	    assertEquals(binding.getDfhcommareaType().getComDate(),
	    		binding2.getDfhcommareaType().getComDate());
	    assertEquals(binding.getDfhcommareaType().getComPersonal().getComAddress(),
	    		binding2.getDfhcommareaType().getComPersonal().getComAddress());
	    assertEquals(binding.getDfhcommareaType().getComPersonal().getComName(),
	    		binding2.getDfhcommareaType().getComPersonal().getComName());
	    assertEquals(binding.getDfhcommareaType().getComPersonal().getComPhone(),
	    		binding2.getDfhcommareaType().getComPersonal().getComPhone());
	}

	public void testContainers() throws Exception {
		LegStarMessageImpl legstarMessage = new LegStarMessageImpl();
	    /* The request java object tree */
	    DfhcommareaType jaxbObject	= CobolTransformerTest.getJaxbObject();
	    DfhcommareaTypeBinding binding = new DfhcommareaTypeBinding(jaxbObject);
	    legstarMessage.addMessagePart(binding, 0, "IBM01147", "CONTAINER-1");
	    DfhcommareaType jaxbObject2	= CobolTransformerTest.getJaxbObject();
	    DfhcommareaTypeBinding binding2 = new DfhcommareaTypeBinding(jaxbObject2);
	    legstarMessage.addMessagePart(binding2, 0, "IBM01147", "CONTAINER-2");
	    assertEquals(2, legstarMessage.getHeaderPart().getDataPartsNumber());
	    
	    Map < String, ICobolComplexBinding > bindingsMap =
	    	new HashMap < String, ICobolComplexBinding >();
	    DfhcommareaTypeBinding binding3 = new DfhcommareaTypeBinding();
	    DfhcommareaTypeBinding binding4 = new DfhcommareaTypeBinding();
	    DfhcommareaTypeBinding binding5 = new DfhcommareaTypeBinding();
	    bindingsMap.put("CONTAINER-1", binding3);
	    bindingsMap.put("CONTAINER-2", binding4);
	    bindingsMap.put("CONTAINER-3", binding5);
	    
	    legstarMessage.getBindingsFromParts(bindingsMap, "IBM01147");
	    
	    DfhcommareaTypeBinding binding6 = (DfhcommareaTypeBinding) bindingsMap.get("CONTAINER-1");
	    assertEquals(100, binding6.getDfhcommareaType().getComNumber());
	    DfhcommareaTypeBinding binding7 = (DfhcommareaTypeBinding) bindingsMap.get("CONTAINER-2");
	    assertEquals(100, binding7.getDfhcommareaType().getComNumber());
	    DfhcommareaTypeBinding binding8 = (DfhcommareaTypeBinding) bindingsMap.get("CONTAINER-3");
	    assertEquals(null, binding8.getDfhcommareaType());
	}
}

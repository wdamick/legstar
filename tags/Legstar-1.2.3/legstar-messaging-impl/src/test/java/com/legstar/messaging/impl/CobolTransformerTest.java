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

import com.legstar.coxb.convert.CobolConverters;
import com.legstar.coxb.host.HostData;
import com.legstar.messaging.impl.CobolTransformer;
import com.legstar.test.coxb.lsfileae.ComPersonal;
import com.legstar.test.coxb.lsfileae.Dfhcommarea;
import com.legstar.test.coxb.lsfileae.bind.DfhcommareaBinding;

import junit.framework.TestCase;

public class CobolTransformerTest extends TestCase {
	
	private static final String HOST_CHARSET = "IBM01140";
	
	private static final String HOST_DATA =
		"f0f0f0f1f0f0e3d6e3d640404040404040404040404040404040d3c1c2c1e240e2e3d9c5c5e34040404040404040f8f8f9f9f3f3f1f4f1f0f0f4f5f84040f0f0f1f0f04bf3f5c140e5d6c9d9404040";
	
	private CobolConverters mCobolConverters;
	
	public void setUp() {
		/* Select a conversion strategy */ 
		mCobolConverters = CobolTransformer.getCobolConverters(HOST_CHARSET);
	}
	
	public void testMarshal() throws Exception {
	    /* The request java object tree */
	    Dfhcommarea jaxbObject	= getJaxbObject();
	    /* Decorate object tree for static binding */
	    DfhcommareaBinding binding = new DfhcommareaBinding(jaxbObject);
	    /* Prepare a byte array to receive the result */
	    byte[] hostBytes = new byte[binding.calcByteLength()];
	    CobolTransformer.marshal(mCobolConverters, binding, hostBytes);
	    assertEquals(HOST_DATA, HostData.toHexString(hostBytes));
	}
	
	public void testUnmarshal() throws Exception {
	    byte[] hostBytes = HostData.toByteArray(HOST_DATA);
	    DfhcommareaBinding binding = new DfhcommareaBinding();
	    CobolTransformer.unmarshal(mCobolConverters, hostBytes, binding);
	    Dfhcommarea jaxbObject = binding.getDfhcommarea();
	    assertEquals(100, jaxbObject.getComNumber());
	    assertEquals("00100.35", jaxbObject.getComAmount());
	    assertEquals("A VOIR", jaxbObject.getComComment());
	    assertEquals("100458", jaxbObject.getComDate());
	    assertEquals("LABAS STREET", jaxbObject.getComPersonal().getComAddress());
	    assertEquals("TOTO", jaxbObject.getComPersonal().getComName());
	    assertEquals("88993314", jaxbObject.getComPersonal().getComPhone());
	}

	/**
	 * This method creates an instance of a JAXB object and sets its properties.
	 * @return a JAXB object
	 */
	public static Dfhcommarea getJaxbObject() {
		Dfhcommarea dfhcommarea = new Dfhcommarea();
		dfhcommarea.setComNumber(100);
		dfhcommarea.setComDate("100458");
		dfhcommarea.setComAmount("00100.35");
		dfhcommarea.setComComment("A VOIR");
		ComPersonal personal = new ComPersonal();
		personal.setComName("TOTO");
		personal.setComAddress("LABAS STREET");
		personal.setComPhone("88993314");
		dfhcommarea.setComPersonal(personal);
		return dfhcommarea;
	}
}

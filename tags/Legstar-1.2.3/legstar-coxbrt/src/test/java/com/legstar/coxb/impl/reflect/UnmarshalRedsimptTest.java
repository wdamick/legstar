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
package com.legstar.coxb.impl.reflect;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.convert.simple.CobolSimpleConverters;
import com.legstar.coxb.impl.reflect.CComplexReflectBinding;
import com.legstar.coxb.impl.visitor.CobolUnmarshalVisitor;
import com.legstar.coxb.util.Utils;
import com.legstar.coxb.host.HostData;
import com.legstar.coxb.host.HostException;

import junit.framework.TestCase;
import com.legstar.test.coxb.redsimpt.ObjectFactory;
import com.legstar.test.coxb.redsimpt.Dfhcommarea;

public class UnmarshalRedsimptTest extends TestCase {
	/**
	 * Unmarshal Redsimpt.
	 * @throws HostException if anything goes wrong
	 * @throws ClassNotFoundException 
	 */
	public final void testRedsimpt() throws HostException, ClassNotFoundException {
		// Create a cobol context 
		CobolContext cobolContext = new CobolContext();
		// Select a conversion strategy 
		CobolSimpleConverters cc = new CobolSimpleConverters(cobolContext);

		// Create an instance of the JAXB object factory
		ObjectFactory objectFactory = new ObjectFactory();

		/* Unmarshal from non-numeric data */
		byte[] hostBytes = HostData.toByteArray("f0f1f2f3f4f5f6f7f8c1f1f2f3f4f5f6f7f8");
		CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0, cc);
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory,
		        Utils.loadClass("com.legstar.test.coxb.redsimpt.Dfhcommarea"));
		ccem.accept(uv);
		Dfhcommarea dfhcommarea = (Dfhcommarea) ccem.getObjectValue(Dfhcommarea.class);
		assertEquals("012345678A12345678", dfhcommarea.getCDefinition1());
		
		/* Unmarshal from numeric data */
		hostBytes = HostData.toByteArray("f8f7f6f5f4f3f2f1f9f8f7f6f5f4f3f2f1f0");
		uv = new CobolUnmarshalVisitor(hostBytes, 0, cc);
		dfhcommarea = objectFactory.createDfhcommarea();
		ccem = new CComplexReflectBinding(objectFactory,
				dfhcommarea);
		ccem.accept(uv);
		assertEquals(new Long(876543219876543210L), dfhcommarea.getCDefinition2());
		
	}

}

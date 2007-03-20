/*******************************************************************************
 * LegStar legacy Web-enablement .
 * Copyright (C)  2007 LegSem
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301  USA
 * 
 *     
 *****************************************************************************/
package com.legstar.cixs.coxb.test;

import junit.framework.TestCase;

import com.legstar.cixs.coxb.CIXSHeader;
import com.legstar.cixs.coxb.CIXSInvokerFactory;
import com.legstar.cixs.coxb.CIXSParameter;
import com.legstar.cixs.coxb.CIXSException;
import com.legstar.cixs.coxb.ICIXSInvoker;
import com.legstar.coxb.reflect.CComplexBinding;
import com.legstar.host.HostException;
import com.legstar.test.coxb.lsfileae.*;

public class LsfileaeClientTest extends TestCase {
	
	public void testDynamic() throws HostException {
		// Create a JAXB object factory
		ObjectFactory oOF = new ObjectFactory();

		/* Create the request JAXB object */
		DfhcommareaType jaxbIn = oOF.createDfhcommareaType();
		
		/* Create a dynamic binding */
		CComplexBinding cein = new CComplexBinding(oOF, jaxbIn);
		
		// Create an input parameter set
		CIXSParameter inParameter;
		
		/* Create a reply JAXB object */
		DfhcommareaType jaxbOut = oOF.createDfhcommareaType();
		
		/* Create a dynamic binding */
		CComplexBinding ceout = new CComplexBinding(oOF, jaxbOut);
		
		// Create an output parameter set
		CIXSParameter outParameter;
		jaxbIn.setComNumber(100);
		jaxbIn.setComDate("100458");
		jaxbIn.setComAmount("00100.35");
		jaxbIn.setComComment("A VOIR");
		ComPersonalType personal = oOF.createComPersonalType();
		personal.setComName("TOTO");
		personal.setComAddress("LABAS STREET");
		personal.setComPhone("88993314");
		jaxbIn.setComPersonal(personal);
		
		try {
			CIXSInvokerFactory cf = new CIXSInvokerFactory();
			ICIXSInvoker invoker = cf.createInvoker();
			inParameter = invoker.createParameter();
			inParameter.setComplexBinding(cein);
			outParameter = invoker.createParameter();
			outParameter.setComplexBinding(ceout);
			CIXSHeader ch = new CIXSHeader();
			invoker.initialize(ch, "lsfileae");
			invoker.invoke(inParameter, outParameter);
			assertEquals(100, jaxbOut.getComNumber());
			assertEquals("26 11 81", jaxbOut.getComDate());
			assertEquals("$0100.11", jaxbOut.getComAmount());
			assertEquals("S. D. BORMAN        ", jaxbOut.getComPersonal().getComName());
			assertEquals("SURREY, ENGLAND     ", jaxbOut.getComPersonal().getComAddress());
			assertEquals("32156778", jaxbOut.getComPersonal().getComPhone());
		}
		catch (CIXSException ce) {
			ce.printStackTrace();
			fail(ce.getMessage());
		}
	}

	public void testStatic() throws HostException {
		// Create a JAXB object factory
		ObjectFactory oOF = new ObjectFactory();

		/* Create the request JAXB object */
		DfhcommareaType jaxbIn = oOF.createDfhcommareaType();
		
		/* Create a static binding */
		com.legstar.test.coxb.lsfileae.bind.DfhcommareaTypeBinding cein =
			new com.legstar.test.coxb.lsfileae.bind.DfhcommareaTypeBinding(oOF, jaxbIn);
		
		// Create an input parameter set
		CIXSParameter inParameter;
		
		/* Create a dynamic binding */
		com.legstar.test.coxb.lsfileae.bind.DfhcommareaTypeBinding ceout =
			new com.legstar.test.coxb.lsfileae.bind.DfhcommareaTypeBinding(oOF);
		
		// Create an output parameter set
		CIXSParameter outParameter;
		jaxbIn.setComNumber(100);
		jaxbIn.setComDate("100458");
		jaxbIn.setComAmount("00100.35");
		jaxbIn.setComComment("A VOIR");
		ComPersonalType personal = oOF.createComPersonalType();
		personal.setComName("TOTO");
		personal.setComAddress("LABAS STREET");
		personal.setComPhone("88993314");
		jaxbIn.setComPersonal(personal);
		
		try {
			CIXSInvokerFactory cf = new CIXSInvokerFactory();
			ICIXSInvoker invoker = cf.createInvoker();
			inParameter = invoker.createParameter();
			inParameter.setComplexBinding(cein);
			outParameter = invoker.createParameter();
			outParameter.setComplexBinding(ceout);
			CIXSHeader ch = new CIXSHeader();
			invoker.initialize(ch, "lsfileae");
			invoker.invoke(inParameter, outParameter);
			/* Create a reply JAXB object */
			DfhcommareaType jaxbOut = ceout.getJaxbObject();
			
			assertEquals(100, jaxbOut.getComNumber());
			assertEquals("26 11 81", jaxbOut.getComDate());
			assertEquals("$0100.11", jaxbOut.getComAmount());
			assertEquals("S. D. BORMAN        ", jaxbOut.getComPersonal().getComName());
			assertEquals("SURREY, ENGLAND     ", jaxbOut.getComPersonal().getComAddress());
			assertEquals("32156778", jaxbOut.getComPersonal().getComPhone());
		}
		catch (CIXSException ce) {
			ce.printStackTrace();
			fail(ce.getMessage());
		}
	}
}

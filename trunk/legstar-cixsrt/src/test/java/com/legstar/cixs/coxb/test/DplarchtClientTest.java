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

import java.util.List;

import junit.framework.TestCase;

import com.legstar.cixs.coxb.CIXSHeader;
import com.legstar.cixs.coxb.CIXSInvokerFactory;
import com.legstar.cixs.coxb.CIXSParameter;
import com.legstar.cixs.coxb.CIXSException;
import com.legstar.cixs.coxb.ICIXSInvoker;
import com.legstar.coxb.reflect.CComplexBinding;
import com.legstar.host.HostException;
import com.legstar.test.coxb.dplarcht.*;

public class DplarchtClientTest extends TestCase {
	
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

		LsRequestType lsRequestType = oOF.createLsRequestType();
		lsRequestType.setLsRequestType(0);
		lsRequestType.setLsAllItems("*");
		
		jaxbIn.setLsRequest(lsRequestType);
		
		try {
			CIXSInvokerFactory cf = new CIXSInvokerFactory();
			ICIXSInvoker invoker = cf.createInvoker();
			inParameter = invoker.createParameter();
			inParameter.setComplexBinding(cein);
			outParameter = invoker.createParameter();
			outParameter.setComplexBinding(ceout);
			CIXSHeader ch = new CIXSHeader();
			invoker.initialize(ch, "dplarcht");
			invoker.invoke(inParameter, outParameter);
			assertEquals(0,jaxbOut.getLsReply().getLsReplyType());
			assertEquals(4,jaxbOut.getLsReply().getLsReplyData().getLsItemsCount());
			List<LsItemsArrayType> arrayType = jaxbOut.getLsReply().getLsReplyData().getLsItemsArray();
			
			assertEquals("CICSTS23.CICS.DFHCSD", arrayType.get(0).getLsFilesData().getLsFileDsname().trim());
			assertEquals("", arrayType.get(1).getLsFilesData().getLsFileDsname().trim());
			assertEquals("CICSTS23.CICS.DFHLRQ", arrayType.get(2).getLsFilesData().getLsFileDsname().trim());
			assertEquals("CICSTS23.CICS.FILEA", arrayType.get(3).getLsFilesData().getLsFileDsname().trim());
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
		
		/* Create a dynamic binding */
		com.legstar.test.coxb.dplarcht.bind.DfhcommareaTypeBinding cein =
			new com.legstar.test.coxb.dplarcht.bind.DfhcommareaTypeBinding(oOF, jaxbIn);
		
		// Create an input parameter set
		CIXSParameter inParameter;
		
		/* Create a static binding */
		com.legstar.test.coxb.dplarcht.bind.DfhcommareaTypeBinding ceout =
			new com.legstar.test.coxb.dplarcht.bind.DfhcommareaTypeBinding(oOF);
		
		// Create an output parameter set
		CIXSParameter outParameter;

		LsRequestType lsRequestType = oOF.createLsRequestType();
		lsRequestType.setLsRequestType(0);
		lsRequestType.setLsAllItems("*");
		
		jaxbIn.setLsRequest(lsRequestType);
		
		try {
			CIXSInvokerFactory cf = new CIXSInvokerFactory();
			ICIXSInvoker invoker = cf.createInvoker();
			CIXSHeader ch = new CIXSHeader();
			inParameter = invoker.createParameter();
			inParameter.setComplexBinding(cein);
			outParameter = invoker.createParameter();
			outParameter.setComplexBinding(ceout);
			invoker.initialize(ch, "dplarcht");
			invoker.invoke(inParameter, outParameter);
			/* Get the reply JAXB object */
			DfhcommareaType jaxbOut = ceout.getJaxbObject();
			
			assertEquals(0,jaxbOut.getLsReply().getLsReplyType());
			assertEquals(4,jaxbOut.getLsReply().getLsReplyData().getLsItemsCount());
			List<LsItemsArrayType> arrayType = jaxbOut.getLsReply().getLsReplyData().getLsItemsArray();
			
			assertEquals("CICSTS23.CICS.DFHCSD", arrayType.get(0).getLsFilesData().getLsFileDsname().trim());
			assertEquals("", arrayType.get(1).getLsFilesData().getLsFileDsname().trim());
			assertEquals("CICSTS23.CICS.DFHLRQ", arrayType.get(2).getLsFilesData().getLsFileDsname().trim());
			assertEquals("CICSTS23.CICS.FILEA", arrayType.get(3).getLsFilesData().getLsFileDsname().trim());
		}
		catch (CIXSException ce) {
			ce.printStackTrace();
			fail(ce.getMessage());
		}
	}
}

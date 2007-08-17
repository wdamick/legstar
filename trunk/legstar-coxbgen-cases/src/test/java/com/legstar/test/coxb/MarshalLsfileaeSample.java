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
import com.legstar.coxb.host.HostData;
import com.legstar.coxb.host.HostException;
import com.legstar.coxb.visitor.CobolMarshalVisitor;
import com.legstar.test.coxb.lsfileae.ComPersonalType;
import com.legstar.test.coxb.lsfileae.DfhcommareaType;
import com.legstar.test.coxb.lsfileae.bind.DfhcommareaTypeBinding;

/**
 * Sample demonstrating how to use a COXB object to marshal java data to
 * z/os data.
 *
 */
public class MarshalLsfileaeSample {

	/**
	 * The main function does not take any argument.
	 * @param args
	 */
	public static void main(String[] args) {
		// Create a JAXB object and set its properties
		DfhcommareaType jaxbObject = getJaxbObject();
		// Create a COXB object wrapping the JAXB object
		DfhcommareaTypeBinding coxbObject = new DfhcommareaTypeBinding(jaxbObject);
		try {
			// Create a buffer to receive the host data
			byte[] hostBytes = new byte[coxbObject.calcByteLength()];
			// Create a marshaler using the default cobol conversion parameters
			CobolMarshalVisitor mv = new CobolMarshalVisitor(hostBytes, 0, new CobolSimpleConverters(new CobolContext()));
			// Marshal the java data to a z/os buffer
			coxbObject.accept(mv);
			// Print the result
			System.out.println("z/os data=" + HostData.toHexString(hostBytes));
			
		} catch (HostException e) {
			e.printStackTrace();
		}
	}
	
	/**
	 * This method creates an instance of a JAXB object and sets its properties.
	 * @return a JAXB object
	 */
	private static DfhcommareaType getJaxbObject() {
		DfhcommareaType dfhcommarea = new DfhcommareaType();
		dfhcommarea.setComNumber(100);
		dfhcommarea.setComDate("100458");
		dfhcommarea.setComAmount("00100.35");
		dfhcommarea.setComComment("A VOIR");
		ComPersonalType personal = new ComPersonalType();
		personal.setComName("TOTO");
		personal.setComAddress("LABAS STREET");
		personal.setComPhone("88993314");
		dfhcommarea.setComPersonal(personal);
		return dfhcommarea;
	}

}

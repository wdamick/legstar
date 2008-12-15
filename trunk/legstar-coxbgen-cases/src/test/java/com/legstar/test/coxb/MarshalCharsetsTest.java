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
import com.legstar.coxb.visitor.CobolMarshalVisitor;
import com.legstar.coxb.convert.simple.CobolSimpleConverters;
import com.legstar.test.coxb.charsets.ObjectFactory;
import com.legstar.test.coxb.charsets.Dfhcommarea;

import com.legstar.coxb.host.HostData;

import junit.framework.TestCase;

public class MarshalCharsetsTest extends TestCase {

    public void testCharsets() throws Exception {

        // Create a cobol context 
        CobolContext cobolContext = new CobolContext();
        cobolContext.setHostCharsetName("IBM01147");
        // Select a conversion strategy 
        CobolSimpleConverters cc = new CobolSimpleConverters(cobolContext);
        // Create a concrete visitor
        byte[] hostBytes = new byte[160];
        CobolMarshalVisitor mv = new CobolMarshalVisitor(hostBytes, 0, cc);

        // Create an instance of the JAXB object factory
        ObjectFactory objectFactory = new ObjectFactory();
        // Create and populate an instance of an object (JAXB annotated)
        Dfhcommarea Dfhcommarea = objectFactory.createDfhcommarea();

        Dfhcommarea.setComLocal("ça c'est un problème");
        Dfhcommarea.setComNational("élémentaire à résoudre");

        // Traverse the object structure, visiting each node with the visitor
        com.legstar.test.coxb.charsets.bind.DfhcommareaBinding ccem = new com.legstar.test.coxb.charsets.bind.DfhcommareaBinding(Dfhcommarea);
        ccem.accept(mv);
        assertEquals("e08140837d85a2a340a495409799968293d094854040404040404040404040400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000e9006c00e9006d0065006e00740061006900720065002000e00020007200e90073006f00750064007200650020002000200020002000200020002000200020",
                HostData.toHexString(mv.getHostBytes()));
    }
}

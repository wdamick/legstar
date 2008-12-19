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
import com.legstar.test.coxb.charsets.Dfhcommarea;
import com.legstar.test.coxb.charsets.bind.DfhcommareaBinding;

import com.legstar.coxb.host.HostData;
import com.legstar.coxb.impl.visitor.CobolMarshalVisitor;
import com.legstar.coxb.test.CharsetsCases;

import junit.framework.TestCase;

/**
 * Marshal charsets.
 *
 */
public class MarshalCharsetsTest extends TestCase {

    /**
     * Marshal host data and test java data object result.
     * @throws Exception if marshaling fails
     */
    public void testCharsets() throws Exception {

        // Create a cobol context 
        CobolContext cobolContext = new CobolContext();
        cobolContext.setHostCharsetName("IBM01147");
        // Select a conversion strategy 
        CobolSimpleConverters cc = new CobolSimpleConverters(cobolContext);
        // Create a concrete visitor
        byte[] hostBytes = new byte[160];
        CobolMarshalVisitor mv = new CobolMarshalVisitor(hostBytes, 0, cc);

        Dfhcommarea dfhcommarea = CharsetsCases.getJavaObject();

        // Traverse the object structure, visiting each node with the visitor
        DfhcommareaBinding ccem = new DfhcommareaBinding(dfhcommarea);
        ccem.accept(mv);
        assertEquals(CharsetsCases.getHostBytesHex(),
                HostData.toHexString(mv.getHostBytes()));
    }
}

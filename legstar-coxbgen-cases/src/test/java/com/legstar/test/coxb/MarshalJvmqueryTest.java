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
import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.coxb.convert.simple.CobolSimpleConverters;
import com.legstar.coxb.host.HostData;
import com.legstar.coxb.impl.visitor.CobolMarshalVisitor;
import com.legstar.test.coxb.jvmquery.bind.JvmQueryReplyBinding;
import com.legstar.xsdc.test.cases.JvmqueryCases;
import com.legstar.xsdc.test.cases.jvmquery.JVMQueryReply;

import junit.framework.TestCase;

/**
 * Marshal jvmquery.
 *
 */
public class MarshalJvmqueryTest extends TestCase {

    /**
     * Marshal host data and test java data object result.
     * @throws Exception if marshaling fails
     */
    public void testJvmquery() throws Exception {

        // Create and populate an instance of an object (JAXB annotated)
        JVMQueryReply jvmQueryReply = JvmqueryCases.getJavaObject();

        ICobolComplexBinding binding = new JvmQueryReplyBinding(jvmQueryReply);
        assertEquals("class com.legstar.xsdc.test.cases.jvmquery.JVMQueryReply", binding.getJaxbType().toString());

        /* Convert Java data object to a host byte array */
        byte[] hostBytes = new byte[binding.calcByteLength()];
        CobolContext cobolContext = new CobolContext();
        CobolSimpleConverters cc = new CobolSimpleConverters(cobolContext);
        CobolMarshalVisitor mv =
            new CobolMarshalVisitor(hostBytes, 0, cc);
        binding.accept(mv);

        /* check */
        assertEquals(196,  mv.getOffset());
        assertEquals(JvmqueryCases.getHostBytesHex().substring(0, 131),
                HostData.toHexString(hostBytes).substring(0, 131));

    }
}

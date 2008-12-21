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
import com.legstar.test.coxb.ws.jvmquery.QueryJvmResponse;
import com.legstar.test.coxb.ws.jvmquery.bind.QueryJvmResponseBinding;

import junit.framework.TestCase;

/**
 * Marshal jvmquery web service.
 *
 */
public class MarshalJvmqueryWsTest extends TestCase {

    /**
     * Marshal host data and test java data object result.
     * @throws Exception if marshaling fails
     */
    public void testJvmqueryWs() throws Exception {

        QueryJvmResponse queryJvmResponse = JvmqueryWsCases.getJavaObject();

        ICobolComplexBinding binding = new QueryJvmResponseBinding(queryJvmResponse);

        /* Convert Java data object to a host byte array */
        byte[] hostBytes = new byte[binding.calcByteLength()];
        CobolContext cobolContext = new CobolContext();
        CobolSimpleConverters cc = new CobolSimpleConverters(cobolContext);
        CobolMarshalVisitor mv =
            new CobolMarshalVisitor(hostBytes, 0, cc);
        binding.accept(mv);

        /* check */
        assertEquals(196,  mv.getOffset());
        assertEquals(JvmqueryWsCases.QUERYJVMRESPONSE_HOST_BYTES.substring(0, 131),
                HostData.toHexString(hostBytes).substring(0, 131));

    }
}

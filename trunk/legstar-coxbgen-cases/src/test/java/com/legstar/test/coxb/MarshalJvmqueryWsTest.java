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
package com.legstar.test.coxb;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.coxb.convert.simple.CobolSimpleConverters;
import com.legstar.coxb.host.HostData;
import com.legstar.coxb.impl.visitor.CobolMarshalVisitor;
import com.legstar.test.coxb.ws.jvmquery.bind.QueryJvmResponseJavaToHostTransformer;
import com.legstar.test.coxb.ws.jvmquery.QueryJvmResponse;
import com.legstar.test.coxb.ws.jvmquery.bind.QueryJvmResponseBinding;

import junit.framework.TestCase;

/**
 * Marshal jvmquery web service.
 *
 */
public class MarshalJvmqueryWsTest extends TestCase {

    /**
     * Marshal java data object and test host data result.
     * @throws Exception if marshaling fails
     */
    public void testJvmqueryWs() throws Exception {

        QueryJvmResponse queryJvmResponse = JvmqueryWsCases.getJavaObject();

        ICobolComplexBinding binding = new QueryJvmResponseBinding(queryJvmResponse);

        /* Convert Java data object to a host byte array */
        byte[] hostBytes = new byte[binding.getByteLength()];
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

    /**
     * Transform java data object and test host data result.
     * @throws Exception if transforming fails
     */
    public void testJavaToHostTransformer() throws Exception {

        QueryJvmResponseJavaToHostTransformer transformer = new QueryJvmResponseJavaToHostTransformer();
        String result = HostData.toHexString(transformer.transform(JvmqueryWsCases.getJavaObject()));
        assertEquals(JvmqueryWsCases.getHostBytesHex().substring(0, 131),
                result.substring(0, 131));
    }
}

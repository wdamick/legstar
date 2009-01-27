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
import com.legstar.test.coxb.jvmquery.bind.JvmQueryReplyJavaToHostTransformer;
import com.legstar.test.coxb.jvmquery.bind.JvmQueryRequestJavaToHostTransformer;
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
     * Marshal java data object and test host data result.
     * @throws Exception if marshaling fails
     */
    public void testJvmquery() throws Exception {

        // Create and populate an instance of an object (JAXB annotated)
        JVMQueryReply jvmQueryReply = JvmqueryCases.getJavaObjectReplyFrance();

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
        assertEquals(JvmqueryCases.getHostBytesHexReplyFrance().substring(0, 131),
                HostData.toHexString(hostBytes).substring(0, 131));

    }

    /**
     * Transform java data object and test host data result.
     * @throws Exception if transforming fails
     */
    public void testJavaToHostTransformer() throws Exception {

        JvmQueryReplyJavaToHostTransformer transformer = new JvmQueryReplyJavaToHostTransformer();
        String result = HostData.toHexString(transformer.transform(JvmqueryCases.getJavaObjectReplyFrance()));
        assertEquals(JvmqueryCases.getHostBytesHexReplyFrance().substring(0, 131),
                result.substring(0, 131));
        
        JvmQueryRequestJavaToHostTransformer transformer2 = new JvmQueryRequestJavaToHostTransformer();
        result = HostData.toHexString(transformer2.transform(JvmqueryCases.getJavaObjectRequest()));
        assertEquals(JvmqueryCases.getHostBytesHexRequest(), result);
        
    }
}

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
import com.legstar.coxb.convert.simple.CobolSimpleConverters;
import com.legstar.test.coxb.charsets.bind.DfhcommareaJavaToHostTransformer;
import com.legstar.test.coxb.charsets.Dfhcommarea;
import com.legstar.test.coxb.charsets.bind.DfhcommareaBinding;

import com.legstar.coxb.host.HostData;
import com.legstar.coxb.impl.visitor.CobolMarshalVisitor;

import junit.framework.TestCase;

/**
 * Marshal charsets.
 *
 */
public class MarshalCharsetsTest extends TestCase {

    /**
     * Marshal java data object and test host data result.
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
    /**
     * Transform java data object and test host data result.
     * @throws Exception if transforming fails
     */
    public void testJavaToHostTransformer() throws Exception {

        DfhcommareaJavaToHostTransformer transformer = new DfhcommareaJavaToHostTransformer("IBM01147");
        assertEquals(CharsetsCases.getHostBytesHex(),
                HostData.toHexString(transformer.transform(CharsetsCases.getJavaObject())));
    }
}

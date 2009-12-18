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
import com.legstar.coxb.host.HostData;
import com.legstar.coxb.impl.visitor.CobolUnmarshalVisitor;
import com.legstar.test.coxb.charsets.bind.DfhcommareaHostToJavaTransformer;
import com.legstar.test.coxb.charsets.bind.DfhcommareaBinding;
import com.legstar.test.coxb.charsets.Dfhcommarea;

import junit.framework.TestCase;

/**
 * Unmarshal charsets.
 *
 */
public class UnmarshalCharsetsTest extends TestCase {

    /**
     * Unmarshal host data and test java data object result.
     * @throws Exception if marshaling fails
     */
    public void testTypesmix() throws Exception {

        // Create a cobol context 
        CobolContext cobolContext = new CobolContext();
        cobolContext.setHostCharsetName("IBM01147");
        // Select a conversion strategy 
        CobolSimpleConverters cc = new CobolSimpleConverters(cobolContext);
        String hexString = CharsetsCases.getHostBytesHex();
        byte[] hostBytes = HostData.toByteArray(hexString);

        // Create a concrete visitor
        CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0, cc);

        // Traverse the object structure, visiting each node with the visitor
        DfhcommareaBinding ccem = new DfhcommareaBinding();
        ccem.accept(uv);
        Dfhcommarea dfhcommarea = ccem.getDfhcommarea();
        CharsetsCases.checkJavaObject(dfhcommarea);
    }
    /**
     * Transform host data and test java data object result.
     * @throws Exception if transforming fails
     */
    public void testHostToJavaTransformer() throws Exception {

        DfhcommareaHostToJavaTransformer transformer = new DfhcommareaHostToJavaTransformer("IBM01147");
        Dfhcommarea dfhcommarea = transformer.transform(HostData.toByteArray(CharsetsCases.getHostBytesHex()));
        CharsetsCases.checkJavaObject(dfhcommarea);
    }
}

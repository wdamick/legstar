/*******************************************************************************
 * Copyright (c) 2011 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.coxb.transform;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.host.HostContext;

import junit.framework.TestCase;

/**
 * Test AbstractTransformer.
 */
public class AbstractTransformerTest extends TestCase {

    /**
     * Test constructors.
     */
    public void testConstructors() {
        HostToJavaLsfileaeTransformer hostToJavaTransformer = new HostToJavaLsfileaeTransformer();
        assertEquals(HostContext.getDefaultHostCharsetName(),
                hostToJavaTransformer.getCobolContext().getHostCharsetName());
        hostToJavaTransformer = new HostToJavaLsfileaeTransformer("IBM01147");
        assertEquals("IBM01147", hostToJavaTransformer.getCobolContext().getHostCharsetName());
        CobolContext cobolContext = new CobolContext();
        cobolContext.setHostCharsetName("IBM01147");
        hostToJavaTransformer = new HostToJavaLsfileaeTransformer(cobolContext);
        assertEquals("IBM01147", hostToJavaTransformer.getCobolContext().getHostCharsetName());
    }
}

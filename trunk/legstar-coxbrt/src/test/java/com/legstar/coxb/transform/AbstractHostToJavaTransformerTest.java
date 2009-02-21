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
package com.legstar.coxb.transform;

import com.legstar.coxb.host.HostData;
import com.legstar.test.coxb.lsfileae.Dfhcommarea;


/**
 * Test AbstractHostToJavaTransformer.
 */
public class AbstractHostToJavaTransformerTest extends AbstractTestTransformers {
    
    /**
     * Test transforming a byte array to java object (using reflection binding).
     * @throws HostTransformException if transform fails
     */
    public void testTransform() throws HostTransformException {
        HostToJavaLsfileaeTransformer hostToJavaTransformer = new HostToJavaLsfileaeTransformer();
        Object javaValue = hostToJavaTransformer.transform(
                HostData.toByteArray(RAW_LSFILEAE_DATA_IBM01147));
        assertTrue(null != javaValue);
        assertTrue(javaValue instanceof Dfhcommarea);
        checkLsfileaeIBM01140((Dfhcommarea) javaValue);
        
        javaValue = hostToJavaTransformer.transform(
                HostData.toByteArray(RAW_LSFILEAE_DATA_IBM01147), STRING_FRENCH_CHARSET);
        assertTrue(null != javaValue);
        assertTrue(javaValue instanceof Dfhcommarea);
        checkLsfileaeIBM01147((Dfhcommarea) javaValue);
    }
    
}

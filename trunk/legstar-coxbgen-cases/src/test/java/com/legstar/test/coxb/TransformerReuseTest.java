/*******************************************************************************
 * Copyright (c) 2010 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.test.coxb;

import com.legstar.coxb.CobolBindingException;
import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.coxb.host.HostData;
import com.legstar.coxb.transform.HostTransformException;
import com.legstar.test.coxb.lsfileae.bind.DfhcommareaJavaToHostTransformer;

import junit.framework.TestCase;

/**
 * Test the caching of transformers per thread.
 *
 */
public class TransformerReuseTest extends TestCase {
    
    
    /**
     * Check that we get the same binding object when a transformer is reused.
     */
    public void testCaching() {
        try {
            DfhcommareaJavaToHostTransformer transformer = new DfhcommareaJavaToHostTransformer();
            ICobolComplexBinding binding = transformer.getCachedBinding();
            assertEquals(LsfileaeCases.getHostBytesHex(),
                    HostData.toHexString(transformer.transform(LsfileaeCases.getJavaObject())));
            ICobolComplexBinding binding2 = transformer.getCachedBinding();
            assertEquals(LsfileaeCases.getHostBytesHex(),
                    HostData.toHexString(transformer.transform(LsfileaeCases.getJavaObject())));
            assertEquals(binding, binding2);
        } catch (HostTransformException e) {
            fail(e.toString());
        } catch (CobolBindingException e) {
            fail(e.toString());
        }
    }

}

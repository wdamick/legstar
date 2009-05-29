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

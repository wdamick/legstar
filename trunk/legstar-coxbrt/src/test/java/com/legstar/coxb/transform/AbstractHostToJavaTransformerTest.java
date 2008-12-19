package com.legstar.coxb.transform;

import com.legstar.coxb.CobolBindingException;
import com.legstar.coxb.CobolContext;
import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.coxb.host.HostData;
import com.legstar.coxb.impl.reflect.CComplexReflectBinding;
import com.legstar.coxb.impl.reflect.ReflectBindingException;
import com.legstar.test.coxb.lsfileae.Dfhcommarea;

import junit.framework.TestCase;

/**
 * Test AbstractHostToJavaTransformer.
 */
public class AbstractHostToJavaTransformerTest extends TestCase {
    
    /** A raw zos serialization. */
    private static final String RAW_LSFILEAE_DATA =
       /* 0 0 0 1 0 0 T O T O                                 L A B A S Á S T R E E T                */
        "f0f0f0f1f0f0e3d6e3d640404040404040404040404040404040d3c1c2c1e2e0e2e3d9c5c5e34040404040404040"
       /* 8 8 9 9 3 3 1 4 1 0 0 4 5 8     0 0 1 0 0 . 3 5 A   V O I R      */
     +  "f8f8f9f9f3f3f1f4f1f0f0f4f5f84040f0f0f1f0f04bf3f5c140e5d6c9d9404040";

    /**
     * Test transforming a byte array to java object (using reflection binding)
     * @throws HostTransformException
     */
    public void testTransform() throws HostTransformException {
        HostToJavaLsfileaeTransformer hostToJavaTransformer = new HostToJavaLsfileaeTransformer();
        Object javaValue = hostToJavaTransformer.transform(HostData.toByteArray(RAW_LSFILEAE_DATA));
        assertTrue(null != javaValue);
        assertTrue(javaValue instanceof Dfhcommarea);
        checkLsfileaeIBM01140((Dfhcommarea) javaValue);
        
        javaValue = hostToJavaTransformer.transform(HostData.toByteArray(RAW_LSFILEAE_DATA), "IBM01147");
        assertTrue(null != javaValue);
        assertTrue(javaValue instanceof Dfhcommarea);
        checkLsfileaeIBM01147((Dfhcommarea) javaValue);
    }
    
    /**
     * Check all member variables from value object with US character set.
     * @param dfhcommarea the value object
     */
    private void checkLsfileaeIBM01140(final Dfhcommarea dfhcommarea) {
        assertEquals("00100.35", dfhcommarea.getComAmount());
        assertEquals("A VOIR", dfhcommarea.getComComment());
        assertEquals("100458", dfhcommarea.getComDate());
        assertEquals(100, dfhcommarea.getComNumber());
        assertEquals("LABAS\\STREET", dfhcommarea.getComPersonal().getComAddress());
        assertEquals("TOTO", dfhcommarea.getComPersonal().getComName());
        assertEquals("88993314", dfhcommarea.getComPersonal().getComPhone());
    }
    
    /**
     * Check all member variables from value object with French character set.
     * @param dfhcommarea the value object
     */
    private void checkLsfileaeIBM01147(final Dfhcommarea dfhcommarea) {
        assertEquals("00100.35", dfhcommarea.getComAmount());
        assertEquals("A VOIR", dfhcommarea.getComComment());
        assertEquals("100458", dfhcommarea.getComDate());
        assertEquals(100, dfhcommarea.getComNumber());
        assertEquals("LABASÁSTREET", dfhcommarea.getComPersonal().getComAddress());
        assertEquals("TOTO", dfhcommarea.getComPersonal().getComName());
        assertEquals("88993314", dfhcommarea.getComPersonal().getComPhone());
    }

    /**
     * An implementation of the abstract class under test.
     *
     */
    public final class HostToJavaLsfileaeTransformer extends AbstractHostToJavaTransformer {

        
        public HostToJavaLsfileaeTransformer() {
            super();
        }
        
        public HostToJavaLsfileaeTransformer(CobolContext cobolContext) {
            super(cobolContext);
        }

        public HostToJavaLsfileaeTransformer(final String hostCharset) {
            super(hostCharset);
        }
        
        @Override
        public ICobolComplexBinding getBinding() throws CobolBindingException {
            try {
                CComplexReflectBinding ccem = new CComplexReflectBinding(
                        new com.legstar.test.coxb.lsfileae.ObjectFactory(), Dfhcommarea.class);
                return ccem;
            } catch (ReflectBindingException e) {
                throw new CobolBindingException(e);
            }
        }
        
    }

}

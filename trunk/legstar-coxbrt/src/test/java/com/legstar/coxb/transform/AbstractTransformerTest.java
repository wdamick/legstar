package com.legstar.coxb.transform;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.coxb.host.HostContext;
import com.legstar.coxb.impl.BindingException;
import com.legstar.coxb.impl.reflect.CComplexReflectBinding;
import com.legstar.coxb.impl.reflect.ReflectBindingException;

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
        assertEquals(HostContext.getDefaultHostCharsetName(), hostToJavaTransformer.getCobolContext().getHostCharsetName());
        hostToJavaTransformer = new HostToJavaLsfileaeTransformer("IBM01147");
        assertEquals("IBM01147", hostToJavaTransformer.getCobolContext().getHostCharsetName());
        CobolContext cobolContext = new CobolContext();
        cobolContext.setHostCharsetName("IBM01147");
        hostToJavaTransformer = new HostToJavaLsfileaeTransformer(cobolContext);
        assertEquals("IBM01147", hostToJavaTransformer.getCobolContext().getHostCharsetName());
    }
    /**
     * An implementation of the abstract class under test.
     *
     */
    public final class HostToJavaLsfileaeTransformer extends AbstractTransformer {

        
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
        public ICobolComplexBinding getBinding() throws BindingException {
            try {
                CComplexReflectBinding ccem = new CComplexReflectBinding(
                        new com.legstar.test.coxb.lsfileae.ObjectFactory(),
                        com.legstar.test.coxb.lsfileae.Dfhcommarea.class);
                return ccem;
            } catch (ReflectBindingException e) {
                throw new BindingException(e);
            }
        }
        
    }
}

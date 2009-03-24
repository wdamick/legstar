package com.legstar.coxb.transform;

import com.legstar.coxb.CobolBindingException;
import com.legstar.coxb.CobolContext;
import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.coxb.impl.reflect.CComplexReflectBinding;
import com.legstar.coxb.impl.reflect.ReflectBindingException;
import com.legstar.test.coxb.lsfileae.Dfhcommarea;

/**
 * An implementation of the abstract class under test.
 * This uses reflection binding.
 *
 */
public final class HostToJavaLsfileaeTransformer extends AbstractHostToJavaTransformer {


    /**
     * No-Arg constructor.
     */
    public HostToJavaLsfileaeTransformer() {
        super();
    }

    /**
     * Create a Host to Java transformer using a specific COBOL parameters set.
     * @param cobolContext the COBOL parameters set.
     */
    public HostToJavaLsfileaeTransformer(final CobolContext cobolContext) {
        super(cobolContext);
    }

    /**
     * Create a Host to Java transformer using a specific host character set while
     * other COBOL parameters are set by default.
     * @param hostCharset the host character set
     */
    public HostToJavaLsfileaeTransformer(final String hostCharset) {
        super(hostCharset);
    }

    /** {@inheritDoc} */
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

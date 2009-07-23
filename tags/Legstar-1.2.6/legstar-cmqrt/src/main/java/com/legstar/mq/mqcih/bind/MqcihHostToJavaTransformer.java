package com.legstar.mq.mqcih.bind;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.coxb.transform.AbstractHostToJavaTransformer;
import com.legstar.coxb.CobolBindingException;

/**
 * Transforms mainframe data to a Mqcih data object.
 * <p/>
 * This is a typical use of this class:
 * <pre>
 *  MqcihHostToJavaTransformer transformer = new MqcihHostToJavaTransformer();
 *  Mqcih javaValue = (Mqcih) transformer.transform(hostByteArray);
 * </pre>
 *
 */
public class MqcihHostToJavaTransformer extends AbstractHostToJavaTransformer {

    
    /**
     * Create a Host to Java transformer using default COBOL parameters.
     */
    public MqcihHostToJavaTransformer() {
        super();
    }
    
    /**
     * Create a Host to Java transformer using a specific host character set while
     * other COBOL parameters are set by default.
     * @param cobolContext the COBOL parameters set.
     */
    public MqcihHostToJavaTransformer(final CobolContext cobolContext) {
        super(cobolContext);
    }

    /**
     * Create a Host to Java transformer using a specific COBOL parameters set.
     * @param hostCharset the host character set
     */
    public MqcihHostToJavaTransformer(final String hostCharset) {
        super(hostCharset);
    }
    
    /**
     * Binding is statically produced by {@link com.legstar.coxb.gen.CoxbBindingGenerator}.
     * @return the binding corresponding to the host structure type.
     * @throws CobolBindingException if binding cannot be returned
     */
    public ICobolComplexBinding getBinding() throws CobolBindingException {
        return new MqcihBinding();
    }
    
}

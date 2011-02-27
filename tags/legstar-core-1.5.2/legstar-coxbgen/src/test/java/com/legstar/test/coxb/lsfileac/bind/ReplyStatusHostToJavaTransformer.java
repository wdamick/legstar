package com.legstar.test.coxb.lsfileac.bind;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.coxb.transform.AbstractHostToJavaTransformer;
import com.legstar.coxb.CobolBindingException;

/**
 * Transforms mainframe data to a ReplyStatus data object.
 * <p/>
 * This is a typical use of this class:
 * <pre>
 *  ReplyStatusHostToJavaTransformer transformer = new ReplyStatusHostToJavaTransformer();
 *  ReplyStatus javaValue = (ReplyStatus) transformer.transform(hostByteArray);
 * </pre>
 *
 */
public class ReplyStatusHostToJavaTransformer extends AbstractHostToJavaTransformer {

    
    /**
     * Create a Host to Java transformer using default COBOL parameters.
     */
    public ReplyStatusHostToJavaTransformer() {
        super();
    }
    
    /**
     * Create a Host to Java transformer using a specific COBOL parameters set.
     * @param cobolContext the COBOL parameters set.
     */
    public ReplyStatusHostToJavaTransformer(final CobolContext cobolContext) {
        super(cobolContext);
    }

    /**
     * Create a Host to Java transformer using a specific host character set while
     * other COBOL parameters are set by default.
     * @param hostCharset the host character set
     */
    public ReplyStatusHostToJavaTransformer(final String hostCharset) {
        super(hostCharset);
    }
    
    /**
     * Binding is statically produced by {@link com.legstar.coxb.gen.CoxbBindingGenerator}.
     * @return a new binding corresponding to the host structure type.
     * @throws CobolBindingException if binding cannot be returned
     */
    public ICobolComplexBinding newBinding() throws CobolBindingException {
        return new ReplyStatusBinding();
    }
    
}

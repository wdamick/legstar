package com.legstar.test.coxb.lsfileal.bind;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.coxb.transform.AbstractJavaToHostTransformer;
import com.legstar.coxb.CobolBindingException;

/**
 * Transforms a RequestParms data object to mainframe data.
 * <p/>
 * This is a typical use of this class:
 * <pre>
 *  RequestParmsJavaToHostTransformer transformer = new RequestParmsJavaToHostTransformer();
 *  byte[] hostByteArray = transformer.transform(javaValue);
 * </pre>
 *
 */
public class RequestParmsJavaToHostTransformer extends AbstractJavaToHostTransformer {

    
    /**
     * Create a Java to Host transformer using default COBOL parameters.
     */
    public RequestParmsJavaToHostTransformer() {
        super();
    }
    
    /**
     * Create a Java to Host transformer using a specific COBOL parameters set.
     * @param cobolContext the COBOL parameters set.
     */
    public RequestParmsJavaToHostTransformer(final CobolContext cobolContext) {
        super(cobolContext);
    }

    /**
     * Create a Java to Host transformer using a specific host character set while
     * other COBOL parameters are set by default.
     * @param hostCharset the host character set
     */
    public RequestParmsJavaToHostTransformer(final String hostCharset) {
        super(hostCharset);
    }
    
    /**
     * Binding is statically produced by {@link com.legstar.coxb.gen.CoxbBindingGenerator}.
     * @return a new binding corresponding to the host structure type.
     * @throws CobolBindingException if binding cannot be returned
     */
    public ICobolComplexBinding newBinding() throws CobolBindingException {
        return new RequestParmsBinding();
    }
    
}

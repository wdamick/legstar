package com.legstar.test.coxb.tcobwvb.bind;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.transform.AbstractHostToJsonTransformer;
import com.legstar.coxb.transform.HostTransformException;

/**
 * Transforms mainframe data to JSON.
 * <p/>
 * This is a typical use of this class:
 *
 * <pre>
 *  CustomerDataHostToJsonTransformer transformer = new CustomerDataHostToJsonTransformer();
 *  StringWriter writer = new StringWriter();
 *  transformer.transform(hostByteArray, writer);
 * </pre>
 *
 */
public class CustomerDataHostToJsonTransformer extends
        AbstractHostToJsonTransformer {

    /**
     * Create a Host to JSON transformer using a Host to Java transformer.
     *
     * @throws HostTransformException if transformer cannot be created
     */
    public CustomerDataHostToJsonTransformer() throws HostTransformException {
        super(new CustomerDataHostToJavaTransformer());
    }
    
    /**
     * Create a Host to JSON transformer using a specific COBOL parameters set.
     *
     * @param cobolContext the COBOL parameters set.
     * @throws HostTransformException if transformer cannot be created
     */
    public CustomerDataHostToJsonTransformer(
            final CobolContext cobolContext) throws HostTransformException {
        super(new CustomerDataHostToJavaTransformer(cobolContext));
    }

    /**
     * Create a Host to JSON transformer using a specific host character set
     * while other COBOL parameters are set by default.
     *
     * @param hostCharset the host character set
     * @throws HostTransformException if transformer cannot be created
     */
    public CustomerDataHostToJsonTransformer(
            final String hostCharset) throws HostTransformException {
        super(new CustomerDataHostToJavaTransformer(hostCharset));
    }
    
}

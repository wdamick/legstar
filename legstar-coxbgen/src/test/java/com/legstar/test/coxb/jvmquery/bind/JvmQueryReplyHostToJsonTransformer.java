package com.legstar.test.coxb.jvmquery.bind;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.transform.AbstractHostToJsonTransformer;
import com.legstar.coxb.transform.HostTransformException;

/**
 * Transforms mainframe data to JSON.
 * <p/>
 * This is a typical use of this class:
 *
 * <pre>
 *  JVMQueryReplyHostToJsonTransformer transformer = new JVMQueryReplyHostToJsonTransformer();
 *  StringWriter writer = new StringWriter();
 *  transformer.transform(hostByteArray, writer);
 * </pre>
 *
 */
public class JvmQueryReplyHostToJsonTransformer extends
        AbstractHostToJsonTransformer {

    /**
     * Create a Host to JSON transformer using a Host to Java transformer.
     *
     * @throws HostTransformException if transformer cannot be created
     */
    public JvmQueryReplyHostToJsonTransformer() throws HostTransformException {
        super(new JvmQueryReplyHostToJavaTransformer());
    }
    
    /**
     * Create a Host to JSON transformer using a specific COBOL parameters set.
     *
     * @param cobolContext the COBOL parameters set.
     * @throws HostTransformException if transformer cannot be created
     */
    public JvmQueryReplyHostToJsonTransformer(
            final CobolContext cobolContext) throws HostTransformException {
        super(new JvmQueryReplyHostToJavaTransformer(cobolContext));
    }

    /**
     * Create a Host to JSON transformer using a specific host character set
     * while other COBOL parameters are set by default.
     *
     * @param hostCharset the host character set
     * @throws HostTransformException if transformer cannot be created
     */
    public JvmQueryReplyHostToJsonTransformer(
            final String hostCharset) throws HostTransformException {
        super(new JvmQueryReplyHostToJavaTransformer(hostCharset));
    }
    
}

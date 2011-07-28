package com.legstar.test.coxb.ws.jvmquery.bind;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.transform.AbstractHostToJsonTransformer;
import com.legstar.coxb.transform.HostTransformException;

/**
 * Transforms mainframe data to JSON.
 * <p/>
 * This is a typical use of this class:
 *
 * <pre>
 *  QueryJvmResponseHostToJsonTransformer transformer = new QueryJvmResponseHostToJsonTransformer();
 *  StringWriter writer = new StringWriter();
 *  transformer.transform(hostByteArray, writer);
 * </pre>
 *
 */
public class QueryJvmResponseHostToJsonTransformer extends
        AbstractHostToJsonTransformer {

    /**
     * Create a Host to JSON transformer using a Host to Java transformer.
     *
     * @throws HostTransformException if transformer cannot be created
     */
    public QueryJvmResponseHostToJsonTransformer() throws HostTransformException {
        super(new QueryJvmResponseHostToJavaTransformer());
    }
    
    /**
     * Create a Host to JSON transformer using a specific COBOL parameters set.
     *
     * @param cobolContext the COBOL parameters set.
     * @throws HostTransformException if transformer cannot be created
     */
    public QueryJvmResponseHostToJsonTransformer(
            final CobolContext cobolContext) throws HostTransformException {
        super(new QueryJvmResponseHostToJavaTransformer(cobolContext));
    }

    /**
     * Create a Host to JSON transformer using a specific host character set
     * while other COBOL parameters are set by default.
     *
     * @param hostCharset the host character set
     * @throws HostTransformException if transformer cannot be created
     */
    public QueryJvmResponseHostToJsonTransformer(
            final String hostCharset) throws HostTransformException {
        super(new QueryJvmResponseHostToJavaTransformer(hostCharset));
    }
    
}

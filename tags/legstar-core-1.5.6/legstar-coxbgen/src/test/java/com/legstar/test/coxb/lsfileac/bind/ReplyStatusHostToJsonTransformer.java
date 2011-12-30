package com.legstar.test.coxb.lsfileac.bind;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.transform.AbstractHostToJsonTransformer;
import com.legstar.coxb.transform.HostTransformException;

/**
 * Transforms mainframe data to JSON.
 * <p/>
 * This is a typical use of this class:
 *
 * <pre>
 *  ReplyStatusHostToJsonTransformer transformer = new ReplyStatusHostToJsonTransformer();
 *  StringWriter writer = new StringWriter();
 *  transformer.transform(hostByteArray, writer);
 * </pre>
 *
 */
public class ReplyStatusHostToJsonTransformer extends
        AbstractHostToJsonTransformer {

    /**
     * Create a Host to JSON transformer using a Host to Java transformer.
     *
     * @throws HostTransformException if transformer cannot be created
     */
    public ReplyStatusHostToJsonTransformer() throws HostTransformException {
        super(new ReplyStatusHostToJavaTransformer());
    }
    
    /**
     * Create a Host to JSON transformer using a specific COBOL parameters set.
     *
     * @param cobolContext the COBOL parameters set.
     * @throws HostTransformException if transformer cannot be created
     */
    public ReplyStatusHostToJsonTransformer(
            final CobolContext cobolContext) throws HostTransformException {
        super(new ReplyStatusHostToJavaTransformer(cobolContext));
    }

    /**
     * Create a Host to JSON transformer using a specific host character set
     * while other COBOL parameters are set by default.
     *
     * @param hostCharset the host character set
     * @throws HostTransformException if transformer cannot be created
     */
    public ReplyStatusHostToJsonTransformer(
            final String hostCharset) throws HostTransformException {
        super(new ReplyStatusHostToJavaTransformer(hostCharset));
    }
    
}

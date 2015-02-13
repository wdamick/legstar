package com.legstar.test.coxb.ardo03.bind;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.transform.AbstractHostToJsonTransformer;
import com.legstar.coxb.transform.HostTransformException;

/**
 * Transforms mainframe data to JSON.
 * <p/>
 * This is a typical use of this class:
 *
 * <pre>
 *  Ardo03RecordHostToJsonTransformer transformer = new Ardo03RecordHostToJsonTransformer();
 *  StringWriter writer = new StringWriter();
 *  transformer.transform(hostByteArray, writer);
 * </pre>
 *
 */
public class Ardo03RecordHostToJsonTransformer extends
        AbstractHostToJsonTransformer {

    /**
     * Create a Host to JSON transformer using a Host to Java transformer.
     *
     * @throws HostTransformException if transformer cannot be created
     */
    public Ardo03RecordHostToJsonTransformer() throws HostTransformException {
        super(new Ardo03RecordHostToJavaTransformer());
    }
    
    /**
     * Create a Host to JSON transformer using a specific COBOL parameters set.
     *
     * @param cobolContext the COBOL parameters set.
     * @throws HostTransformException if transformer cannot be created
     */
    public Ardo03RecordHostToJsonTransformer(
            final CobolContext cobolContext) throws HostTransformException {
        super(new Ardo03RecordHostToJavaTransformer(cobolContext));
    }

    /**
     * Create a Host to JSON transformer using a specific host character set
     * while other COBOL parameters are set by default.
     *
     * @param hostCharset the host character set
     * @throws HostTransformException if transformer cannot be created
     */
    public Ardo03RecordHostToJsonTransformer(
            final String hostCharset) throws HostTransformException {
        super(new Ardo03RecordHostToJavaTransformer(hostCharset));
    }
    
}

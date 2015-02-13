package com.legstar.test.coxb.ardo02.bind;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.transform.AbstractJsonToHostTransformer;
import com.legstar.coxb.transform.HostTransformException;

/**
 * Transforms JSON to mainframe data.
 * <p/>
 * This is a typical use of this class:
 *
 * <pre>
 *  StringReader reader = new StringReader("{someJson:...}");
 *  Ardo02RecordJsonToHostTransformer transformer = new Ardo02RecordJsonToHostTransformer();
 *  byte[] hostByteArray = transformer.transform(reader);
 * </pre>
 *
 */
public class Ardo02RecordJsonToHostTransformer extends
        AbstractJsonToHostTransformer {

    
    /**
     * Create a JSON to Host transformer using a Java to Host transformer.
     *
     * @throws HostTransformException if transformer cannot be created
     */
    public Ardo02RecordJsonToHostTransformer() throws HostTransformException {
        super(new Ardo02RecordJavaToHostTransformer());
    }
    
    /**
     * Create an JSON to Host transformer using a specific COBOL parameters set.
     *
     * @param cobolContext the COBOL parameters set.
     * @throws HostTransformException if transformer cannot be created
     */
    public Ardo02RecordJsonToHostTransformer(
            final CobolContext cobolContext) throws HostTransformException {
        super(new Ardo02RecordJavaToHostTransformer(cobolContext));
    }

    /**
     * Create an JSON to Host transformer using a specific host character set
     * while other COBOL parameters are set by default.
     *
     * @param hostCharset the host character set
     * @throws HostTransformException if transformer cannot be created
     */
    public Ardo02RecordJsonToHostTransformer(
            final String hostCharset) throws HostTransformException {
        super(new Ardo02RecordJavaToHostTransformer(hostCharset));
    }
    
}

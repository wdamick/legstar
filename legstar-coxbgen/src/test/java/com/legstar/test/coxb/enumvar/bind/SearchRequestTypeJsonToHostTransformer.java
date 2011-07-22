package com.legstar.test.coxb.enumvar.bind;

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
 *  SearchRequestTypeJsonToHostTransformer transformer = new SearchRequestTypeJsonToHostTransformer();
 *  byte[] hostByteArray = transformer.transform(reader);
 * </pre>
 *
 */
public class SearchRequestTypeJsonToHostTransformer extends
        AbstractJsonToHostTransformer {

    
    /**
     * Create a JSON to Host transformer using a Java to Host transformer.
     *
     * @throws HostTransformException if transformer cannot be created
     */
    public SearchRequestTypeJsonToHostTransformer() throws HostTransformException {
        super(new SearchRequestTypeJavaToHostTransformer());
    }
    
    /**
     * Create an JSON to Host transformer using a specific COBOL parameters set.
     *
     * @param cobolContext the COBOL parameters set.
     * @throws HostTransformException if transformer cannot be created
     */
    public SearchRequestTypeJsonToHostTransformer(
            final CobolContext cobolContext) throws HostTransformException {
        super(new SearchRequestTypeJavaToHostTransformer(cobolContext));
    }

    /**
     * Create an JSON to Host transformer using a specific host character set
     * while other COBOL parameters are set by default.
     *
     * @param hostCharset the host character set
     * @throws HostTransformException if transformer cannot be created
     */
    public SearchRequestTypeJsonToHostTransformer(
            final String hostCharset) throws HostTransformException {
        super(new SearchRequestTypeJavaToHostTransformer(hostCharset));
    }
    
}

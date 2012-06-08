package com.legstar.test.coxb.lsfileal.bind;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.transform.AbstractHostToXmlTransformer;
import com.legstar.coxb.transform.HostTransformException;

/**
 * Transforms mainframe data to XML.
 * <p/>
 * This is a typical use of this class:
 * <pre>
 *  RequestParmsHostToXmlTransformer transformer = new RequestParmsHostToXmlTransformer();
 *  StringWriter writer = new StringWriter();
 *  transformer.transform(hostByteArray, writer);
 * </pre>
 *
 */
public class RequestParmsHostToXmlTransformer extends AbstractHostToXmlTransformer {

    /**
     * Create a Host to XML transformer using a Host to Java transformer.
     * @throws HostTransformException if transformer cannot be created
     */
    public RequestParmsHostToXmlTransformer() throws HostTransformException {
        super(new RequestParmsHostToJavaTransformer());
    }
    
    /**
     * Create a Host to XML transformer using a specific COBOL parameters set.
     * @param cobolContext the COBOL parameters set.
     * @throws HostTransformException if transformer cannot be created
     */
    public RequestParmsHostToXmlTransformer(
            final CobolContext cobolContext) throws HostTransformException {
        super(new RequestParmsHostToJavaTransformer(cobolContext));
    }

    /**
     * Create a Host to XML transformer using a specific host character set while
     * other COBOL parameters are set by default.
     * @param hostCharset the host character set
     * @throws HostTransformException if transformer cannot be created
     */
    public RequestParmsHostToXmlTransformer(
            final String hostCharset) throws HostTransformException {
        super(new RequestParmsHostToJavaTransformer(hostCharset));
    }
    
    /** {@inheritDoc} */
    public String getElementName() {
        return "RequestParms";
    }

    /** {@inheritDoc} */
    public String getNamespace() {
        return "http://legstar.com/test/coxb/lsfileal";
    }

    /** {@inheritDoc} */
    public boolean isXmlRootElement() {
        return false;
    }
    
}

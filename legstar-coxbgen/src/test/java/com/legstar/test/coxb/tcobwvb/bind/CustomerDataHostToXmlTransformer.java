package com.legstar.test.coxb.tcobwvb.bind;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.transform.AbstractHostToXmlTransformer;
import com.legstar.coxb.transform.HostTransformException;

/**
 * Transforms mainframe data to XML.
 * <p/>
 * This is a typical use of this class:
 * <pre>
 *  CustomerDataHostToXmlTransformer transformer = new CustomerDataHostToXmlTransformer();
 *  StringWriter writer = new StringWriter();
 *  transformer.transform(hostByteArray, writer);
 * </pre>
 *
 */
public class CustomerDataHostToXmlTransformer extends AbstractHostToXmlTransformer {

    /**
     * Create a Host to XML transformer using a Host to Java transformer.
     * @throws HostTransformException if transformer cannot be created
     */
    public CustomerDataHostToXmlTransformer() throws HostTransformException {
        super(new CustomerDataHostToJavaTransformer());
    }
    
    /**
     * Create a Host to XML transformer using a specific COBOL parameters set.
     * @param cobolContext the COBOL parameters set.
     * @throws HostTransformException if transformer cannot be created
     */
    public CustomerDataHostToXmlTransformer(
            final CobolContext cobolContext) throws HostTransformException {
        super(new CustomerDataHostToJavaTransformer(cobolContext));
    }

    /**
     * Create a Host to XML transformer using a specific host character set while
     * other COBOL parameters are set by default.
     * @param hostCharset the host character set
     * @throws HostTransformException if transformer cannot be created
     */
    public CustomerDataHostToXmlTransformer(
            final String hostCharset) throws HostTransformException {
        super(new CustomerDataHostToJavaTransformer(hostCharset));
    }
    
    /** {@inheritDoc} */
    public String getElementName() {
        return "CustomerData";
    }

    /** {@inheritDoc} */
    public String getNamespace() {
        return "http://legstar.com/test/coxb/tcobwvb";
    }

    /** {@inheritDoc} */
    public boolean isXmlRootElement() {
        return false;
    }
    
}

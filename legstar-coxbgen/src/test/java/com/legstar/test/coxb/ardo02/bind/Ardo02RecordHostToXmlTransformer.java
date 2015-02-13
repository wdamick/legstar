package com.legstar.test.coxb.ardo02.bind;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.transform.AbstractHostToXmlTransformer;
import com.legstar.coxb.transform.HostTransformException;

/**
 * Transforms mainframe data to XML.
 * <p/>
 * This is a typical use of this class:
 * <pre>
 *  Ardo02RecordHostToXmlTransformer transformer = new Ardo02RecordHostToXmlTransformer();
 *  StringWriter writer = new StringWriter();
 *  transformer.transform(hostByteArray, writer);
 * </pre>
 *
 */
public class Ardo02RecordHostToXmlTransformer extends AbstractHostToXmlTransformer {

    /**
     * Create a Host to XML transformer using a Host to Java transformer.
     * @throws HostTransformException if transformer cannot be created
     */
    public Ardo02RecordHostToXmlTransformer() throws HostTransformException {
        super(new Ardo02RecordHostToJavaTransformer());
    }
    
    /**
     * Create a Host to XML transformer using a specific COBOL parameters set.
     * @param cobolContext the COBOL parameters set.
     * @throws HostTransformException if transformer cannot be created
     */
    public Ardo02RecordHostToXmlTransformer(
            final CobolContext cobolContext) throws HostTransformException {
        super(new Ardo02RecordHostToJavaTransformer(cobolContext));
    }

    /**
     * Create a Host to XML transformer using a specific host character set while
     * other COBOL parameters are set by default.
     * @param hostCharset the host character set
     * @throws HostTransformException if transformer cannot be created
     */
    public Ardo02RecordHostToXmlTransformer(
            final String hostCharset) throws HostTransformException {
        super(new Ardo02RecordHostToJavaTransformer(hostCharset));
    }
    
    /** {@inheritDoc} */
    public String getElementName() {
        return "Ardo02Record";
    }

    /** {@inheritDoc} */
    public String getNamespace() {
        return "http://legstar.com/test/coxb/ardo02";
    }

    /** {@inheritDoc} */
    public boolean isXmlRootElement() {
        return false;
    }
    
}

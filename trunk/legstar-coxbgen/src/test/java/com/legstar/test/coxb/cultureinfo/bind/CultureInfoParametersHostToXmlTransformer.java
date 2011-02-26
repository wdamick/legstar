package com.legstar.test.coxb.cultureinfo.bind;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.transform.AbstractHostToXmlTransformer;
import com.legstar.coxb.transform.HostTransformException;

/**
 * Transforms mainframe data to XML.
 * <p/>
 * This is a typical use of this class:
 * <pre>
 *  CultureInfoParametersHostToXmlTransformer transformer = new CultureInfoParametersHostToXmlTransformer();
 *  StringWriter writer = new StringWriter();
 *  transformer.transform(hostByteArray, writer);
 * </pre>
 *
 */
public class CultureInfoParametersHostToXmlTransformer extends AbstractHostToXmlTransformer {

    /**
     * Create a Host to XML transformer using a Host to Java transformer.
     * @throws HostTransformException if transformer cannot be created
     */
    public CultureInfoParametersHostToXmlTransformer() throws HostTransformException {
        super(new CultureInfoParametersHostToJavaTransformer());
    }
    
    /**
     * Create a Host to XML transformer using a specific COBOL parameters set.
     * @param cobolContext the COBOL parameters set.
     * @throws HostTransformException if transformer cannot be created
     */
    public CultureInfoParametersHostToXmlTransformer(
            final CobolContext cobolContext) throws HostTransformException {
        super(new CultureInfoParametersHostToJavaTransformer(cobolContext));
    }

    /**
     * Create a Host to XML transformer using a specific host character set while
     * other COBOL parameters are set by default.
     * @param hostCharset the host character set
     * @throws HostTransformException if transformer cannot be created
     */
    public CultureInfoParametersHostToXmlTransformer(
            final String hostCharset) throws HostTransformException {
        super(new CultureInfoParametersHostToJavaTransformer(hostCharset));
    }
    
    /** {@inheritDoc} */
    public String getElementName() {
        return "cultureInfoParameters";
    }

    /** {@inheritDoc} */
    public String getNamespace() {
        return "http://cultureinfo.cases.test.xsdc.legstar.com/";
    }

    /** {@inheritDoc} */
    public boolean isXmlRootElement() {
        return false;
    }
    
}

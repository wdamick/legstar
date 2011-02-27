package com.legstar.test.coxb.enumvar.bind;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.transform.AbstractHostToXmlTransformer;
import com.legstar.coxb.transform.HostTransformException;

/**
 * Transforms mainframe data to XML.
 * <p/>
 * This is a typical use of this class:
 * <pre>
 *  SearchRequestTypeHostToXmlTransformer transformer = new SearchRequestTypeHostToXmlTransformer();
 *  StringWriter writer = new StringWriter();
 *  transformer.transform(hostByteArray, writer);
 * </pre>
 *
 */
public class SearchRequestTypeHostToXmlTransformer extends AbstractHostToXmlTransformer {

    /**
     * Create a Host to XML transformer using a Host to Java transformer.
     * @throws HostTransformException if transformer cannot be created
     */
    public SearchRequestTypeHostToXmlTransformer() throws HostTransformException {
        super(new SearchRequestTypeHostToJavaTransformer());
    }
    
    /**
     * Create a Host to XML transformer using a specific COBOL parameters set.
     * @param cobolContext the COBOL parameters set.
     * @throws HostTransformException if transformer cannot be created
     */
    public SearchRequestTypeHostToXmlTransformer(
            final CobolContext cobolContext) throws HostTransformException {
        super(new SearchRequestTypeHostToJavaTransformer(cobolContext));
    }

    /**
     * Create a Host to XML transformer using a specific host character set while
     * other COBOL parameters are set by default.
     * @param hostCharset the host character set
     * @throws HostTransformException if transformer cannot be created
     */
    public SearchRequestTypeHostToXmlTransformer(
            final String hostCharset) throws HostTransformException {
        super(new SearchRequestTypeHostToJavaTransformer(hostCharset));
    }
    
    /** {@inheritDoc} */
    public String getElementName() {
        return "SearchRequest";
    }

    /** {@inheritDoc} */
    public String getNamespace() {
        return "http://legstar.com/test/coxb/enumvar";
    }

    /** {@inheritDoc} */
    public boolean isXmlRootElement() {
        return false;
    }
    
}

package com.legstar.test.coxb.MSNSearch.bind;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.transform.AbstractHostToXmlTransformer;
import com.legstar.coxb.transform.HostTransformException;

/**
 * Transforms mainframe data to XML.
 * <p/>
 * This is a typical use of this class:
 * <pre>
 *  SearchResponseHostToXmlTransformer transformer = new SearchResponseHostToXmlTransformer();
 *  StringWriter writer = new StringWriter();
 *  transformer.transform(hostByteArray, writer);
 * </pre>
 *
 */
public class SearchResponseHostToXmlTransformer extends AbstractHostToXmlTransformer {

    /**
     * Create a Host to XML transformer using a Host to Java transformer.
     * @throws HostTransformException if transformer cannot be created
     */
    public SearchResponseHostToXmlTransformer() throws HostTransformException {
        super(new SearchResponseHostToJavaTransformer());
    }
    
    /**
     * Create a Host to XML transformer using a specific COBOL parameters set.
     * @param cobolContext the COBOL parameters set.
     * @throws HostTransformException if transformer cannot be created
     */
    public SearchResponseHostToXmlTransformer(
            final CobolContext cobolContext) throws HostTransformException {
        super(new SearchResponseHostToJavaTransformer(cobolContext));
    }

    /**
     * Create a Host to XML transformer using a specific host character set while
     * other COBOL parameters are set by default.
     * @param hostCharset the host character set
     * @throws HostTransformException if transformer cannot be created
     */
    public SearchResponseHostToXmlTransformer(
            final String hostCharset) throws HostTransformException {
        super(new SearchResponseHostToJavaTransformer(hostCharset));
    }
    
    /** {@inheritDoc} */
    public String getElementName() {
        return "SearchResponse";
    }

    /** {@inheritDoc} */
    public String getNamespace() {
        return "http://schemas.microsoft.com/MSNSearch/2005/09/fex";
    }

    /** {@inheritDoc} */
    public boolean isXmlRootElement() {
        return true;
    }
    
}

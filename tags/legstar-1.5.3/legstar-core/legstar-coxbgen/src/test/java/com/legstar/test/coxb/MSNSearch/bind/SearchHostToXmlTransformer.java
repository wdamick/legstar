package com.legstar.test.coxb.MSNSearch.bind;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.transform.AbstractHostToXmlTransformer;
import com.legstar.coxb.transform.HostTransformException;

/**
 * Transforms mainframe data to XML.
 * <p/>
 * This is a typical use of this class:
 * <pre>
 *  SearchHostToXmlTransformer transformer = new SearchHostToXmlTransformer();
 *  StringWriter writer = new StringWriter();
 *  transformer.transform(hostByteArray, writer);
 * </pre>
 *
 */
public class SearchHostToXmlTransformer extends AbstractHostToXmlTransformer {

    /**
     * Create a Host to XML transformer using a Host to Java transformer.
     * @throws HostTransformException if transformer cannot be created
     */
    public SearchHostToXmlTransformer() throws HostTransformException {
        super(new SearchHostToJavaTransformer());
    }
    
    /**
     * Create a Host to XML transformer using a specific COBOL parameters set.
     * @param cobolContext the COBOL parameters set.
     * @throws HostTransformException if transformer cannot be created
     */
    public SearchHostToXmlTransformer(
            final CobolContext cobolContext) throws HostTransformException {
        super(new SearchHostToJavaTransformer(cobolContext));
    }

    /**
     * Create a Host to XML transformer using a specific host character set while
     * other COBOL parameters are set by default.
     * @param hostCharset the host character set
     * @throws HostTransformException if transformer cannot be created
     */
    public SearchHostToXmlTransformer(
            final String hostCharset) throws HostTransformException {
        super(new SearchHostToJavaTransformer(hostCharset));
    }
    
    /** {@inheritDoc} */
    public String getElementName() {
        return "Search";
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

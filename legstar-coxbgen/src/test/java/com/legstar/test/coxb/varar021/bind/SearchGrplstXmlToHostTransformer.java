package com.legstar.test.coxb.varar021.bind;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.transform.AbstractXmlToHostTransformer;
import com.legstar.coxb.transform.HostTransformException;

/**
 * Transforms XML to mainframe data.
 * <p/>
 * This is a typical use of this class:
 * <pre>
 *  StringReader reader = new StringReader("<someXml>...</someXml>");
 *  SearchGrplstXmlToHostTransformer transformer = new SearchGrplstXmlToHostTransformer();
 *  byte[] hostByteArray = transformer.transform(new StreamSource(reader));
 * </pre>
 *
 */
public class SearchGrplstXmlToHostTransformer extends AbstractXmlToHostTransformer {

    
    /**
     * Create a XML to Host transformer using a Java to Host transformer.
     * @throws HostTransformException if transformer cannot be created
     */
    public SearchGrplstXmlToHostTransformer() throws HostTransformException {
        super(new SearchGrplstJavaToHostTransformer());
    }
    
    /**
     * Create an XML to Host transformer using a specific COBOL parameters set.
     * @param cobolContext the COBOL parameters set.
     * @throws HostTransformException if transformer cannot be created
     */
    public SearchGrplstXmlToHostTransformer(
            final CobolContext cobolContext) throws HostTransformException {
        super(new SearchGrplstJavaToHostTransformer(cobolContext));
    }

    /**
     * Create an XML to Host transformer using a specific host character set while
     * other COBOL parameters are set by default.
     * @param hostCharset the host character set
     * @throws HostTransformException if transformer cannot be created
     */
    public SearchGrplstXmlToHostTransformer(
            final String hostCharset) throws HostTransformException {
        super(new SearchGrplstJavaToHostTransformer(hostCharset));
    }
    
}

package com.legstar.test.coxb.ws.jvmquery.bind;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.transform.AbstractHostToXmlTransformer;
import com.legstar.coxb.transform.HostTransformException;

/**
 * Transforms mainframe data to XML.
 * <p/>
 * This is a typical use of this class:
 * <pre>
 *  QueryJvmHostToXmlTransformer transformer = new QueryJvmHostToXmlTransformer();
 *  StringWriter writer = new StringWriter();
 *  transformer.transform(hostByteArray, writer);
 * </pre>
 *
 */
public class QueryJvmHostToXmlTransformer extends AbstractHostToXmlTransformer {

    /**
     * Create a Host to XML transformer using a Host to Java transformer.
     * @throws HostTransformException if transformer cannot be created
     */
    public QueryJvmHostToXmlTransformer() throws HostTransformException {
        super(new QueryJvmHostToJavaTransformer());
    }
    
    /**
     * Create a Host to XML transformer using a specific COBOL parameters set.
     * @param cobolContext the COBOL parameters set.
     * @throws HostTransformException if transformer cannot be created
     */
    public QueryJvmHostToXmlTransformer(
            final CobolContext cobolContext) throws HostTransformException {
        super(new QueryJvmHostToJavaTransformer(cobolContext));
    }

    /**
     * Create a Host to XML transformer using a specific host character set while
     * other COBOL parameters are set by default.
     * @param hostCharset the host character set
     * @throws HostTransformException if transformer cannot be created
     */
    public QueryJvmHostToXmlTransformer(
            final String hostCharset) throws HostTransformException {
        super(new QueryJvmHostToJavaTransformer(hostCharset));
    }
    
    /** {@inheritDoc} */
    public String getElementName() {
        return "queryJvm";
    }

    /** {@inheritDoc} */
    public String getNamespace() {
        return "http://jvmquery.ws.cases.test.xsdc.legstar.com/";
    }

    /** {@inheritDoc} */
    public boolean isXmlRootElement() {
        return false;
    }
    
}

package com.legstar.test.coxb.ws.jvmquery.bind;

import com.legstar.coxb.transform.AbstractXmlTransformers;
import com.legstar.coxb.transform.HostTransformException;

/**
 * XML Transformers provider for QueryJvmResponse.
 *
 */
public class QueryJvmResponseXmlTransformers extends AbstractXmlTransformers {

    /**
     * Create a set of directional transformers.
     * @throws HostTransformException if transformer cannot be created
     */
    public QueryJvmResponseXmlTransformers() throws HostTransformException {
        super(new QueryJvmResponseXmlToHostTransformer(),
                new QueryJvmResponseHostToXmlTransformer());
    }

}

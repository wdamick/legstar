package com.legstar.test.coxb.ws.jvmquery.bind;

import com.legstar.coxb.transform.AbstractXmlTransformers;
import com.legstar.coxb.transform.HostTransformException;

/**
 * XML Transformers provider for QueryJvm.
 *
 */
public class QueryJvmXmlTransformers extends AbstractXmlTransformers {

    /**
     * Create a set of directional transformers.
     * @throws HostTransformException if transformer cannot be created
     */
    public QueryJvmXmlTransformers() throws HostTransformException {
        super(new QueryJvmXmlToHostTransformer(),
                new QueryJvmHostToXmlTransformer());
    }

}

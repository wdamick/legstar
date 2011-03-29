package com.legstar.test.coxb.lsfileac.bind;

import com.legstar.coxb.transform.AbstractXmlTransformers;
import com.legstar.coxb.transform.HostTransformException;

/**
 * XML Transformers provider for QueryData.
 *
 */
public class QueryDataXmlTransformers extends AbstractXmlTransformers {

    /**
     * Create a set of directional transformers.
     * @throws HostTransformException if transformer cannot be created
     */
    public QueryDataXmlTransformers() throws HostTransformException {
        super(new QueryDataXmlToHostTransformer(),
                new QueryDataHostToXmlTransformer());
    }

}

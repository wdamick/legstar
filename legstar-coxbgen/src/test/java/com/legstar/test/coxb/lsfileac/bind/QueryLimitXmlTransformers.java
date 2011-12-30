package com.legstar.test.coxb.lsfileac.bind;

import com.legstar.coxb.transform.AbstractXmlTransformers;
import com.legstar.coxb.transform.HostTransformException;

/**
 * XML Transformers provider for QueryLimit.
 *
 */
public class QueryLimitXmlTransformers extends AbstractXmlTransformers {

    /**
     * Create a set of directional transformers.
     * @throws HostTransformException if transformer cannot be created
     */
    public QueryLimitXmlTransformers() throws HostTransformException {
        super(new QueryLimitXmlToHostTransformer(),
                new QueryLimitHostToXmlTransformer());
    }

}

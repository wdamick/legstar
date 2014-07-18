package com.legstar.test.coxb.lsfileac.bind;

import com.legstar.coxb.transform.AbstractJsonTransformers;
import com.legstar.coxb.transform.HostTransformException;

/**
 * JSON Transformers provider for QueryLimit.
 *
 */
public class QueryLimitJsonTransformers extends AbstractJsonTransformers {

    /**
     * Create a set of directional transformers.
     *
     * @throws HostTransformException if transformer cannot be created
     */
    public QueryLimitJsonTransformers() throws HostTransformException {
        super(new QueryLimitJsonToHostTransformer(),
                new QueryLimitHostToJsonTransformer());
    }

}

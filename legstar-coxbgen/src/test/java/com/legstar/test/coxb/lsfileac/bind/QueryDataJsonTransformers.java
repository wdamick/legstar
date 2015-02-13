package com.legstar.test.coxb.lsfileac.bind;

import com.legstar.coxb.transform.AbstractJsonTransformers;
import com.legstar.coxb.transform.HostTransformException;

/**
 * JSON Transformers provider for QueryData.
 *
 */
public class QueryDataJsonTransformers extends AbstractJsonTransformers {

    /**
     * Create a set of directional transformers.
     *
     * @throws HostTransformException if transformer cannot be created
     */
    public QueryDataJsonTransformers() throws HostTransformException {
        super(new QueryDataJsonToHostTransformer(),
                new QueryDataHostToJsonTransformer());
    }

}

package com.legstar.test.coxb.ws.jvmquery.bind;

import com.legstar.coxb.transform.AbstractJsonTransformers;
import com.legstar.coxb.transform.HostTransformException;

/**
 * JSON Transformers provider for QueryJvmResponse.
 *
 */
public class QueryJvmResponseJsonTransformers extends AbstractJsonTransformers {

    /**
     * Create a set of directional transformers.
     *
     * @throws HostTransformException if transformer cannot be created
     */
    public QueryJvmResponseJsonTransformers() throws HostTransformException {
        super(new QueryJvmResponseJsonToHostTransformer(),
                new QueryJvmResponseHostToJsonTransformer());
    }

}

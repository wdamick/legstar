package com.legstar.test.coxb.ws.jvmquery.bind;

import com.legstar.coxb.transform.AbstractJsonTransformers;
import com.legstar.coxb.transform.HostTransformException;

/**
 * JSON Transformers provider for QueryJvm.
 *
 */
public class QueryJvmJsonTransformers extends AbstractJsonTransformers {

    /**
     * Create a set of directional transformers.
     *
     * @throws HostTransformException if transformer cannot be created
     */
    public QueryJvmJsonTransformers() throws HostTransformException {
        super(new QueryJvmJsonToHostTransformer(),
                new QueryJvmHostToJsonTransformer());
    }

}

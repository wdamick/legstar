package com.legstar.test.coxb.lsfileal.bind;

import com.legstar.coxb.transform.AbstractJsonTransformers;
import com.legstar.coxb.transform.HostTransformException;

/**
 * JSON Transformers provider for RequestParms.
 *
 */
public class RequestParmsJsonTransformers extends AbstractJsonTransformers {

    /**
     * Create a set of directional transformers.
     *
     * @throws HostTransformException if transformer cannot be created
     */
    public RequestParmsJsonTransformers() throws HostTransformException {
        super(new RequestParmsJsonToHostTransformer(),
                new RequestParmsHostToJsonTransformer());
    }

}

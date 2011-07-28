package com.legstar.test.coxb.rq071.bind;

import com.legstar.coxb.transform.AbstractJsonTransformers;
import com.legstar.coxb.transform.HostTransformException;

/**
 * JSON Transformers provider for RQ071Input.
 *
 */
public class RQ071InputJsonTransformers extends AbstractJsonTransformers {

    /**
     * Create a set of directional transformers.
     *
     * @throws HostTransformException if transformer cannot be created
     */
    public RQ071InputJsonTransformers() throws HostTransformException {
        super(new RQ071InputJsonToHostTransformer(),
                new RQ071InputHostToJsonTransformer());
    }

}

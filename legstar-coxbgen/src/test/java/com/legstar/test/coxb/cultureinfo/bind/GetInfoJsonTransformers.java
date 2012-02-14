package com.legstar.test.coxb.cultureinfo.bind;

import com.legstar.coxb.transform.AbstractJsonTransformers;
import com.legstar.coxb.transform.HostTransformException;

/**
 * JSON Transformers provider for GetInfo.
 *
 */
public class GetInfoJsonTransformers extends AbstractJsonTransformers {

    /**
     * Create a set of directional transformers.
     *
     * @throws HostTransformException if transformer cannot be created
     */
    public GetInfoJsonTransformers() throws HostTransformException {
        super(new GetInfoJsonToHostTransformer(),
                new GetInfoHostToJsonTransformer());
    }

}

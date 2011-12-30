package com.legstar.test.coxb.cultureinfo.bind;

import com.legstar.coxb.transform.AbstractJsonTransformers;
import com.legstar.coxb.transform.HostTransformException;

/**
 * JSON Transformers provider for GetInfoResponse.
 *
 */
public class GetInfoResponseJsonTransformers extends AbstractJsonTransformers {

    /**
     * Create a set of directional transformers.
     *
     * @throws HostTransformException if transformer cannot be created
     */
    public GetInfoResponseJsonTransformers() throws HostTransformException {
        super(new GetInfoResponseJsonToHostTransformer(),
                new GetInfoResponseHostToJsonTransformer());
    }

}

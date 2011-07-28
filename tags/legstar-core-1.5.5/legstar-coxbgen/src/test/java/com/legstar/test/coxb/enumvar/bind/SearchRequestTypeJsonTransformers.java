package com.legstar.test.coxb.enumvar.bind;

import com.legstar.coxb.transform.AbstractJsonTransformers;
import com.legstar.coxb.transform.HostTransformException;

/**
 * JSON Transformers provider for SearchRequestType.
 *
 */
public class SearchRequestTypeJsonTransformers extends AbstractJsonTransformers {

    /**
     * Create a set of directional transformers.
     *
     * @throws HostTransformException if transformer cannot be created
     */
    public SearchRequestTypeJsonTransformers() throws HostTransformException {
        super(new SearchRequestTypeJsonToHostTransformer(),
                new SearchRequestTypeHostToJsonTransformer());
    }

}

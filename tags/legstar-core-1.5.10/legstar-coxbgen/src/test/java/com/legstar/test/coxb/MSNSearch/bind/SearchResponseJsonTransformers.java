package com.legstar.test.coxb.MSNSearch.bind;

import com.legstar.coxb.transform.AbstractJsonTransformers;
import com.legstar.coxb.transform.HostTransformException;

/**
 * JSON Transformers provider for SearchResponse.
 *
 */
public class SearchResponseJsonTransformers extends AbstractJsonTransformers {

    /**
     * Create a set of directional transformers.
     *
     * @throws HostTransformException if transformer cannot be created
     */
    public SearchResponseJsonTransformers() throws HostTransformException {
        super(new SearchResponseJsonToHostTransformer(),
                new SearchResponseHostToJsonTransformer());
    }

}

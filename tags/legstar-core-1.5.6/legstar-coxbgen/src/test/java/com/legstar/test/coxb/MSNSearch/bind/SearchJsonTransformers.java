package com.legstar.test.coxb.MSNSearch.bind;

import com.legstar.coxb.transform.AbstractJsonTransformers;
import com.legstar.coxb.transform.HostTransformException;

/**
 * JSON Transformers provider for Search.
 *
 */
public class SearchJsonTransformers extends AbstractJsonTransformers {

    /**
     * Create a set of directional transformers.
     *
     * @throws HostTransformException if transformer cannot be created
     */
    public SearchJsonTransformers() throws HostTransformException {
        super(new SearchJsonToHostTransformer(),
                new SearchHostToJsonTransformer());
    }

}

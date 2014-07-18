package com.legstar.test.coxb.varar021.bind;

import com.legstar.coxb.transform.AbstractJsonTransformers;
import com.legstar.coxb.transform.HostTransformException;

/**
 * JSON Transformers provider for SearchGrplst.
 *
 */
public class SearchGrplstJsonTransformers extends AbstractJsonTransformers {

    /**
     * Create a set of directional transformers.
     *
     * @throws HostTransformException if transformer cannot be created
     */
    public SearchGrplstJsonTransformers() throws HostTransformException {
        super(new SearchGrplstJsonToHostTransformer(),
                new SearchGrplstHostToJsonTransformer());
    }

}

package com.legstar.test.coxb.cultureinfo.bind;

import com.legstar.coxb.transform.AbstractJsonTransformers;
import com.legstar.coxb.transform.HostTransformException;

/**
 * JSON Transformers provider for CultureInfoParameters.
 *
 */
public class CultureInfoParametersJsonTransformers extends AbstractJsonTransformers {

    /**
     * Create a set of directional transformers.
     *
     * @throws HostTransformException if transformer cannot be created
     */
    public CultureInfoParametersJsonTransformers() throws HostTransformException {
        super(new CultureInfoParametersJsonToHostTransformer(),
                new CultureInfoParametersHostToJsonTransformer());
    }

}

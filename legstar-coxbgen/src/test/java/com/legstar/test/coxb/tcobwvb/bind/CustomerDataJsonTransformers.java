package com.legstar.test.coxb.tcobwvb.bind;

import com.legstar.coxb.transform.AbstractJsonTransformers;
import com.legstar.coxb.transform.HostTransformException;

/**
 * JSON Transformers provider for CustomerData.
 *
 */
public class CustomerDataJsonTransformers extends AbstractJsonTransformers {

    /**
     * Create a set of directional transformers.
     *
     * @throws HostTransformException if transformer cannot be created
     */
    public CustomerDataJsonTransformers() throws HostTransformException {
        super(new CustomerDataJsonToHostTransformer(),
                new CustomerDataHostToJsonTransformer());
    }

}

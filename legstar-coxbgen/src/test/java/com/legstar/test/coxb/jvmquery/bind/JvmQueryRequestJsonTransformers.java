package com.legstar.test.coxb.jvmquery.bind;

import com.legstar.coxb.transform.AbstractJsonTransformers;
import com.legstar.coxb.transform.HostTransformException;

/**
 * JSON Transformers provider for JVMQueryRequest.
 *
 */
public class JvmQueryRequestJsonTransformers extends AbstractJsonTransformers {

    /**
     * Create a set of directional transformers.
     *
     * @throws HostTransformException if transformer cannot be created
     */
    public JvmQueryRequestJsonTransformers() throws HostTransformException {
        super(new JvmQueryRequestJsonToHostTransformer(),
                new JvmQueryRequestHostToJsonTransformer());
    }

}

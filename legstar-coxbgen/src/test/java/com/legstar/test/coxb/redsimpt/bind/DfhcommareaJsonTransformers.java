package com.legstar.test.coxb.redsimpt.bind;

import com.legstar.coxb.transform.AbstractJsonTransformers;
import com.legstar.coxb.transform.HostTransformException;

/**
 * JSON Transformers provider for Dfhcommarea.
 *
 */
public class DfhcommareaJsonTransformers extends AbstractJsonTransformers {

    /**
     * Create a set of directional transformers.
     *
     * @throws HostTransformException if transformer cannot be created
     */
    public DfhcommareaJsonTransformers() throws HostTransformException {
        super(new DfhcommareaJsonToHostTransformer(),
                new DfhcommareaHostToJsonTransformer());
    }

}

package com.legstar.test.coxb.ardo02.bind;

import com.legstar.coxb.transform.AbstractJsonTransformers;
import com.legstar.coxb.transform.HostTransformException;

/**
 * JSON Transformers provider for Ardo02Record.
 *
 */
public class Ardo02RecordJsonTransformers extends AbstractJsonTransformers {

    /**
     * Create a set of directional transformers.
     *
     * @throws HostTransformException if transformer cannot be created
     */
    public Ardo02RecordJsonTransformers() throws HostTransformException {
        super(new Ardo02RecordJsonToHostTransformer(),
                new Ardo02RecordHostToJsonTransformer());
    }

}

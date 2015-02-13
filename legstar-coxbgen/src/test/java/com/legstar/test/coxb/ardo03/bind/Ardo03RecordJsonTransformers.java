package com.legstar.test.coxb.ardo03.bind;

import com.legstar.coxb.transform.AbstractJsonTransformers;
import com.legstar.coxb.transform.HostTransformException;

/**
 * JSON Transformers provider for Ardo03Record.
 *
 */
public class Ardo03RecordJsonTransformers extends AbstractJsonTransformers {

    /**
     * Create a set of directional transformers.
     *
     * @throws HostTransformException if transformer cannot be created
     */
    public Ardo03RecordJsonTransformers() throws HostTransformException {
        super(new Ardo03RecordJsonToHostTransformer(),
                new Ardo03RecordHostToJsonTransformer());
    }

}

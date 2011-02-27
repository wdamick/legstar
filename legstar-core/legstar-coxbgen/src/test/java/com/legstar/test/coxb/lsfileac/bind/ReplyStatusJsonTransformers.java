package com.legstar.test.coxb.lsfileac.bind;

import com.legstar.coxb.transform.AbstractJsonTransformers;
import com.legstar.coxb.transform.HostTransformException;

/**
 * JSON Transformers provider for ReplyStatus.
 *
 */
public class ReplyStatusJsonTransformers extends AbstractJsonTransformers {

    /**
     * Create a set of directional transformers.
     *
     * @throws HostTransformException if transformer cannot be created
     */
    public ReplyStatusJsonTransformers() throws HostTransformException {
        super(new ReplyStatusJsonToHostTransformer(),
                new ReplyStatusHostToJsonTransformer());
    }

}

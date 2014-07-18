package com.legstar.test.coxb.lsfileal.bind;

import com.legstar.coxb.transform.AbstractJsonTransformers;
import com.legstar.coxb.transform.HostTransformException;

/**
 * JSON Transformers provider for ReplyData.
 *
 */
public class ReplyDataJsonTransformers extends AbstractJsonTransformers {

    /**
     * Create a set of directional transformers.
     *
     * @throws HostTransformException if transformer cannot be created
     */
    public ReplyDataJsonTransformers() throws HostTransformException {
        super(new ReplyDataJsonToHostTransformer(),
                new ReplyDataHostToJsonTransformer());
    }

}

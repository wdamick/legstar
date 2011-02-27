package com.legstar.test.coxb.cultureinfo.bind;

import com.legstar.coxb.transform.AbstractJsonTransformers;
import com.legstar.coxb.transform.HostTransformException;

/**
 * JSON Transformers provider for CultureInfoReply.
 *
 */
public class CultureInfoReplyJsonTransformers extends AbstractJsonTransformers {

    /**
     * Create a set of directional transformers.
     *
     * @throws HostTransformException if transformer cannot be created
     */
    public CultureInfoReplyJsonTransformers() throws HostTransformException {
        super(new CultureInfoReplyJsonToHostTransformer(),
                new CultureInfoReplyHostToJsonTransformer());
    }

}

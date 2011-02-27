package com.legstar.test.coxb.jvmquery.bind;

import com.legstar.coxb.transform.AbstractJsonTransformers;
import com.legstar.coxb.transform.HostTransformException;

/**
 * JSON Transformers provider for JVMQueryReply.
 *
 */
public class JvmQueryReplyJsonTransformers extends AbstractJsonTransformers {

    /**
     * Create a set of directional transformers.
     *
     * @throws HostTransformException if transformer cannot be created
     */
    public JvmQueryReplyJsonTransformers() throws HostTransformException {
        super(new JvmQueryReplyJsonToHostTransformer(),
                new JvmQueryReplyHostToJsonTransformer());
    }

}

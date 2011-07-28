package com.legstar.test.coxb.jvmquery.bind;

import com.legstar.coxb.transform.AbstractXmlTransformers;
import com.legstar.coxb.transform.HostTransformException;

/**
 * XML Transformers provider for JVMQueryReply.
 *
 */
public class JvmQueryReplyXmlTransformers extends AbstractXmlTransformers {

    /**
     * Create a set of directional transformers.
     * @throws HostTransformException if transformer cannot be created
     */
    public JvmQueryReplyXmlTransformers() throws HostTransformException {
        super(new JvmQueryReplyXmlToHostTransformer(),
                new JvmQueryReplyHostToXmlTransformer());
    }

}

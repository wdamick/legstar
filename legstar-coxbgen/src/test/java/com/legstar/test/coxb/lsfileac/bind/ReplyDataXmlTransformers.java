package com.legstar.test.coxb.lsfileac.bind;

import com.legstar.coxb.transform.AbstractXmlTransformers;
import com.legstar.coxb.transform.HostTransformException;

/**
 * XML Transformers provider for ReplyData.
 *
 */
public class ReplyDataXmlTransformers extends AbstractXmlTransformers {

    /**
     * Create a set of directional transformers.
     * @throws HostTransformException if transformer cannot be created
     */
    public ReplyDataXmlTransformers() throws HostTransformException {
        super(new ReplyDataXmlToHostTransformer(),
                new ReplyDataHostToXmlTransformer());
    }

}

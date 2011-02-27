package com.legstar.test.coxb.lsfileac.bind;

import com.legstar.coxb.transform.AbstractXmlTransformers;
import com.legstar.coxb.transform.HostTransformException;

/**
 * XML Transformers provider for ReplyStatus.
 *
 */
public class ReplyStatusXmlTransformers extends AbstractXmlTransformers {

    /**
     * Create a set of directional transformers.
     * @throws HostTransformException if transformer cannot be created
     */
    public ReplyStatusXmlTransformers() throws HostTransformException {
        super(new ReplyStatusXmlToHostTransformer(),
                new ReplyStatusHostToXmlTransformer());
    }

}

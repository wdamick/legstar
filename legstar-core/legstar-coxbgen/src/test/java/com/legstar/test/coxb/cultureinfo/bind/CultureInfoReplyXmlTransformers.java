package com.legstar.test.coxb.cultureinfo.bind;

import com.legstar.coxb.transform.AbstractXmlTransformers;
import com.legstar.coxb.transform.HostTransformException;

/**
 * XML Transformers provider for CultureInfoReply.
 *
 */
public class CultureInfoReplyXmlTransformers extends AbstractXmlTransformers {

    /**
     * Create a set of directional transformers.
     * @throws HostTransformException if transformer cannot be created
     */
    public CultureInfoReplyXmlTransformers() throws HostTransformException {
        super(new CultureInfoReplyXmlToHostTransformer(),
                new CultureInfoReplyHostToXmlTransformer());
    }

}

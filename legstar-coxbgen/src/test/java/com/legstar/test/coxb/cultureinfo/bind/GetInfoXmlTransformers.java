package com.legstar.test.coxb.cultureinfo.bind;

import com.legstar.coxb.transform.AbstractXmlTransformers;
import com.legstar.coxb.transform.HostTransformException;

/**
 * XML Transformers provider for GetInfo.
 *
 */
public class GetInfoXmlTransformers extends AbstractXmlTransformers {

    /**
     * Create a set of directional transformers.
     * @throws HostTransformException if transformer cannot be created
     */
    public GetInfoXmlTransformers() throws HostTransformException {
        super(new GetInfoXmlToHostTransformer(),
                new GetInfoHostToXmlTransformer());
    }

}

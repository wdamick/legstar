package com.legstar.test.coxb.cultureinfo.bind;

import com.legstar.coxb.transform.AbstractXmlTransformers;
import com.legstar.coxb.transform.HostTransformException;

/**
 * XML Transformers provider for GetInfoResponse.
 *
 */
public class GetInfoResponseXmlTransformers extends AbstractXmlTransformers {

    /**
     * Create a set of directional transformers.
     * @throws HostTransformException if transformer cannot be created
     */
    public GetInfoResponseXmlTransformers() throws HostTransformException {
        super(new GetInfoResponseXmlToHostTransformer(),
                new GetInfoResponseHostToXmlTransformer());
    }

}

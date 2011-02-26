package com.legstar.test.coxb.lsfileal.bind;

import com.legstar.coxb.transform.AbstractXmlTransformers;
import com.legstar.coxb.transform.HostTransformException;

/**
 * XML Transformers provider for RequestParms.
 *
 */
public class RequestParmsXmlTransformers extends AbstractXmlTransformers {

    /**
     * Create a set of directional transformers.
     * @throws HostTransformException if transformer cannot be created
     */
    public RequestParmsXmlTransformers() throws HostTransformException {
        super(new RequestParmsXmlToHostTransformer(),
                new RequestParmsHostToXmlTransformer());
    }

}

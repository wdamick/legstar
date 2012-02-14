package com.legstar.test.coxb.rq071.bind;

import com.legstar.coxb.transform.AbstractXmlTransformers;
import com.legstar.coxb.transform.HostTransformException;

/**
 * XML Transformers provider for RQ071Output.
 *
 */
public class RQ071OutputXmlTransformers extends AbstractXmlTransformers {

    /**
     * Create a set of directional transformers.
     * @throws HostTransformException if transformer cannot be created
     */
    public RQ071OutputXmlTransformers() throws HostTransformException {
        super(new RQ071OutputXmlToHostTransformer(),
                new RQ071OutputHostToXmlTransformer());
    }

}

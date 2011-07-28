package com.legstar.test.coxb.rq071.bind;

import com.legstar.coxb.transform.AbstractXmlTransformers;
import com.legstar.coxb.transform.HostTransformException;

/**
 * XML Transformers provider for RQ071Input.
 *
 */
public class RQ071InputXmlTransformers extends AbstractXmlTransformers {

    /**
     * Create a set of directional transformers.
     * @throws HostTransformException if transformer cannot be created
     */
    public RQ071InputXmlTransformers() throws HostTransformException {
        super(new RQ071InputXmlToHostTransformer(),
                new RQ071InputHostToXmlTransformer());
    }

}

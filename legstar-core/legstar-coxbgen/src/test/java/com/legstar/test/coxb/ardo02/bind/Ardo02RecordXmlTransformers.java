package com.legstar.test.coxb.ardo02.bind;

import com.legstar.coxb.transform.AbstractXmlTransformers;
import com.legstar.coxb.transform.HostTransformException;

/**
 * XML Transformers provider for Ardo02Record.
 *
 */
public class Ardo02RecordXmlTransformers extends AbstractXmlTransformers {

    /**
     * Create a set of directional transformers.
     * @throws HostTransformException if transformer cannot be created
     */
    public Ardo02RecordXmlTransformers() throws HostTransformException {
        super(new Ardo02RecordXmlToHostTransformer(),
                new Ardo02RecordHostToXmlTransformer());
    }

}

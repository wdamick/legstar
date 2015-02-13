package com.legstar.test.coxb.ardo03.bind;

import com.legstar.coxb.transform.AbstractXmlTransformers;
import com.legstar.coxb.transform.HostTransformException;

/**
 * XML Transformers provider for Ardo03Record.
 *
 */
public class Ardo03RecordXmlTransformers extends AbstractXmlTransformers {

    /**
     * Create a set of directional transformers.
     * @throws HostTransformException if transformer cannot be created
     */
    public Ardo03RecordXmlTransformers() throws HostTransformException {
        super(new Ardo03RecordXmlToHostTransformer(),
                new Ardo03RecordHostToXmlTransformer());
    }

}

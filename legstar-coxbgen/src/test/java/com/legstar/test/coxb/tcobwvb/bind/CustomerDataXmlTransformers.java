package com.legstar.test.coxb.tcobwvb.bind;

import com.legstar.coxb.transform.AbstractXmlTransformers;
import com.legstar.coxb.transform.HostTransformException;

/**
 * XML Transformers provider for CustomerData.
 *
 */
public class CustomerDataXmlTransformers extends AbstractXmlTransformers {

    /**
     * Create a set of directional transformers.
     * @throws HostTransformException if transformer cannot be created
     */
    public CustomerDataXmlTransformers() throws HostTransformException {
        super(new CustomerDataXmlToHostTransformer(),
                new CustomerDataHostToXmlTransformer());
    }

}

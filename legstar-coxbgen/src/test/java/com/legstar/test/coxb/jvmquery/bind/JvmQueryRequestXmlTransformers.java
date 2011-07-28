package com.legstar.test.coxb.jvmquery.bind;

import com.legstar.coxb.transform.AbstractXmlTransformers;
import com.legstar.coxb.transform.HostTransformException;

/**
 * XML Transformers provider for JVMQueryRequest.
 *
 */
public class JvmQueryRequestXmlTransformers extends AbstractXmlTransformers {

    /**
     * Create a set of directional transformers.
     * @throws HostTransformException if transformer cannot be created
     */
    public JvmQueryRequestXmlTransformers() throws HostTransformException {
        super(new JvmQueryRequestXmlToHostTransformer(),
                new JvmQueryRequestHostToXmlTransformer());
    }

}

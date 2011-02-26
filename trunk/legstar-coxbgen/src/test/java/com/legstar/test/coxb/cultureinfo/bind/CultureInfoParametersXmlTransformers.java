package com.legstar.test.coxb.cultureinfo.bind;

import com.legstar.coxb.transform.AbstractXmlTransformers;
import com.legstar.coxb.transform.HostTransformException;

/**
 * XML Transformers provider for CultureInfoParameters.
 *
 */
public class CultureInfoParametersXmlTransformers extends AbstractXmlTransformers {

    /**
     * Create a set of directional transformers.
     * @throws HostTransformException if transformer cannot be created
     */
    public CultureInfoParametersXmlTransformers() throws HostTransformException {
        super(new CultureInfoParametersXmlToHostTransformer(),
                new CultureInfoParametersHostToXmlTransformer());
    }

}

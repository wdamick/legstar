package com.legstar.test.coxb.enumvar.bind;

import com.legstar.coxb.transform.AbstractXmlTransformers;
import com.legstar.coxb.transform.HostTransformException;

/**
 * XML Transformers provider for SearchRequestType.
 *
 */
public class SearchRequestTypeXmlTransformers extends AbstractXmlTransformers {

    /**
     * Create a set of directional transformers.
     * @throws HostTransformException if transformer cannot be created
     */
    public SearchRequestTypeXmlTransformers() throws HostTransformException {
        super(new SearchRequestTypeXmlToHostTransformer(),
                new SearchRequestTypeHostToXmlTransformer());
    }

}

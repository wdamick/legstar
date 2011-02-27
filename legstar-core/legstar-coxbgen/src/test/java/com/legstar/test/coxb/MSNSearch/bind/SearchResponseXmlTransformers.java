package com.legstar.test.coxb.MSNSearch.bind;

import com.legstar.coxb.transform.AbstractXmlTransformers;
import com.legstar.coxb.transform.HostTransformException;

/**
 * XML Transformers provider for SearchResponse.
 *
 */
public class SearchResponseXmlTransformers extends AbstractXmlTransformers {

    /**
     * Create a set of directional transformers.
     * @throws HostTransformException if transformer cannot be created
     */
    public SearchResponseXmlTransformers() throws HostTransformException {
        super(new SearchResponseXmlToHostTransformer(),
                new SearchResponseHostToXmlTransformer());
    }

}

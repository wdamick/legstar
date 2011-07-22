package com.legstar.test.coxb.MSNSearch.bind;

import com.legstar.coxb.transform.AbstractXmlTransformers;
import com.legstar.coxb.transform.HostTransformException;

/**
 * XML Transformers provider for Search.
 *
 */
public class SearchXmlTransformers extends AbstractXmlTransformers {

    /**
     * Create a set of directional transformers.
     * @throws HostTransformException if transformer cannot be created
     */
    public SearchXmlTransformers() throws HostTransformException {
        super(new SearchXmlToHostTransformer(),
                new SearchHostToXmlTransformer());
    }

}

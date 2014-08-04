package com.legstar.test.coxb.varar021.bind;

import com.legstar.coxb.transform.AbstractXmlTransformers;
import com.legstar.coxb.transform.HostTransformException;

/**
 * XML Transformers provider for SearchGrplst.
 *
 */
public class SearchGrplstXmlTransformers extends AbstractXmlTransformers {

    /**
     * Create a set of directional transformers.
     * @throws HostTransformException if transformer cannot be created
     */
    public SearchGrplstXmlTransformers() throws HostTransformException {
        super(new SearchGrplstXmlToHostTransformer(),
                new SearchGrplstHostToXmlTransformer());
    }

}

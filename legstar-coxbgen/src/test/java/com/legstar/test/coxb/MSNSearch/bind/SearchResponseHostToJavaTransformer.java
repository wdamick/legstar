package com.legstar.test.coxb.MSNSearch.bind;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.coxb.transform.AbstractHostToJavaTransformer;
import com.legstar.coxb.CobolBindingException;

/**
 * Transforms mainframe data to a SearchResponse data object.
 * <p/>
 * This is a typical use of this class:
 * <pre>
 *  SearchResponseHostToJavaTransformer transformer = new SearchResponseHostToJavaTransformer();
 *  SearchResponse javaValue = (SearchResponse) transformer.transform(hostByteArray);
 * </pre>
 *
 */
public class SearchResponseHostToJavaTransformer extends AbstractHostToJavaTransformer {

    
    /**
     * Create a Host to Java transformer using default COBOL parameters.
     */
    public SearchResponseHostToJavaTransformer() {
        super();
    }
    
    /**
     * Create a Host to Java transformer using a specific COBOL parameters set.
     * @param cobolContext the COBOL parameters set.
     */
    public SearchResponseHostToJavaTransformer(final CobolContext cobolContext) {
        super(cobolContext);
    }

    /**
     * Create a Host to Java transformer using a specific host character set while
     * other COBOL parameters are set by default.
     * @param hostCharset the host character set
     */
    public SearchResponseHostToJavaTransformer(final String hostCharset) {
        super(hostCharset);
    }
    
    /**
     * Binding is statically produced by {@link com.legstar.coxb.gen.CoxbBindingGenerator}.
     * @return a new binding corresponding to the host structure type.
     * @throws CobolBindingException if binding cannot be returned
     */
    public ICobolComplexBinding newBinding() throws CobolBindingException {
        return new SearchResponseBinding();
    }
    
}

package com.legstar.test.cixs.numzoned;

import javax.xml.bind.annotation.XmlRegistry;

/**
 * A JAXB ObjectFactory for wrapper classes.
 *
 */
@XmlRegistry
public class ObjectFactory {


    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes
     * for package: com.legstar.test.cixs.numzoned.
     * 
     */
    public ObjectFactory() {
    }

    /**
     * @return an instance of {@link NumzonedHostHeader }
     * 
     */
    public NumzonedHostHeader createNumzonedHostHeader() {
        return new NumzonedHostHeader();
    }

    /**
     * @return an instance of {@link NumzonedRequest }
     * 
     */
    public NumzonedRequest createNumzonedRequest() {
        return new NumzonedRequest();
    }

    /**
     * @return an instance of {@link NumzonedResponse }
     * 
     */
    public NumzonedResponse createNumzonedResponse() {
        return new NumzonedResponse();
    }


    /**
     * @return an instance of {@link NumzonedFaultInfo }
     * 
     */
    public NumzonedFaultInfo createNumzonedFaultInfo() {
        return new NumzonedFaultInfo();
    }

}

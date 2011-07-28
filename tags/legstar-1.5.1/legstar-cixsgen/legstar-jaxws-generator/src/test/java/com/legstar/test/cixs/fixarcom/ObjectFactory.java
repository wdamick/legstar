package com.legstar.test.cixs.fixarcom;

import javax.xml.bind.annotation.XmlRegistry;

/**
 * A JAXB ObjectFactory for wrapper classes.
 *
 */
@XmlRegistry
public class ObjectFactory {


    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes
     * for package: com.legstar.test.cixs.fixarcom.
     * 
     */
    public ObjectFactory() {
    }

    /**
     * @return an instance of {@link FixarcomHostHeader }
     * 
     */
    public FixarcomHostHeader createFixarcomHostHeader() {
        return new FixarcomHostHeader();
    }

    /**
     * @return an instance of {@link FixarcomRequest }
     * 
     */
    public FixarcomRequest createFixarcomRequest() {
        return new FixarcomRequest();
    }

    /**
     * @return an instance of {@link FixarcomResponse }
     * 
     */
    public FixarcomResponse createFixarcomResponse() {
        return new FixarcomResponse();
    }


    /**
     * @return an instance of {@link FixarcomFaultInfo }
     * 
     */
    public FixarcomFaultInfo createFixarcomFaultInfo() {
        return new FixarcomFaultInfo();
    }

}

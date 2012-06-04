package com.legstar.test.cixs.valuemix;

import javax.xml.bind.annotation.XmlRegistry;

/**
 * A JAXB ObjectFactory for wrapper classes.
 *
 */
@XmlRegistry
public class ObjectFactory {


    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes
     * for package: com.legstar.test.cixs.valuemix.
     * 
     */
    public ObjectFactory() {
    }

    /**
     * @return an instance of {@link ValuemixHostHeader }
     * 
     */
    public ValuemixHostHeader createValuemixHostHeader() {
        return new ValuemixHostHeader();
    }

    /**
     * @return an instance of {@link ValuemixRequest }
     * 
     */
    public ValuemixRequest createValuemixRequest() {
        return new ValuemixRequest();
    }

    /**
     * @return an instance of {@link ValuemixResponse }
     * 
     */
    public ValuemixResponse createValuemixResponse() {
        return new ValuemixResponse();
    }


    /**
     * @return an instance of {@link ValuemixFaultInfo }
     * 
     */
    public ValuemixFaultInfo createValuemixFaultInfo() {
        return new ValuemixFaultInfo();
    }

}

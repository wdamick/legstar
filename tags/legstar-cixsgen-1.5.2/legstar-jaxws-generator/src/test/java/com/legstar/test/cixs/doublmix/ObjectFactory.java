package com.legstar.test.cixs.doublmix;

import javax.xml.bind.annotation.XmlRegistry;

/**
 * A JAXB ObjectFactory for wrapper classes.
 *
 */
@XmlRegistry
public class ObjectFactory {


    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes
     * for package: com.legstar.test.cixs.doublmix.
     * 
     */
    public ObjectFactory() {
    }

    /**
     * @return an instance of {@link DoublmixHostHeader }
     * 
     */
    public DoublmixHostHeader createDoublmixHostHeader() {
        return new DoublmixHostHeader();
    }

    /**
     * @return an instance of {@link DoublmixRequest }
     * 
     */
    public DoublmixRequest createDoublmixRequest() {
        return new DoublmixRequest();
    }

    /**
     * @return an instance of {@link DoublmixResponse }
     * 
     */
    public DoublmixResponse createDoublmixResponse() {
        return new DoublmixResponse();
    }


    /**
     * @return an instance of {@link DoublmixFaultInfo }
     * 
     */
    public DoublmixFaultInfo createDoublmixFaultInfo() {
        return new DoublmixFaultInfo();
    }

}

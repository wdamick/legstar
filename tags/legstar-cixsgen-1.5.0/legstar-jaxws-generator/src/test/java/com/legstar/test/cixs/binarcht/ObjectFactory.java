package com.legstar.test.cixs.binarcht;

import javax.xml.bind.annotation.XmlRegistry;

/**
 * A JAXB ObjectFactory for wrapper classes.
 *
 */
@XmlRegistry
public class ObjectFactory {


    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes
     * for package: com.legstar.test.cixs.binarcht.
     * 
     */
    public ObjectFactory() {
    }

    /**
     * @return an instance of {@link BinarchtHostHeader }
     * 
     */
    public BinarchtHostHeader createBinarchtHostHeader() {
        return new BinarchtHostHeader();
    }

    /**
     * @return an instance of {@link BinarchtRequest }
     * 
     */
    public BinarchtRequest createBinarchtRequest() {
        return new BinarchtRequest();
    }

    /**
     * @return an instance of {@link BinarchtResponse }
     * 
     */
    public BinarchtResponse createBinarchtResponse() {
        return new BinarchtResponse();
    }


    /**
     * @return an instance of {@link BinarchtFaultInfo }
     * 
     */
    public BinarchtFaultInfo createBinarchtFaultInfo() {
        return new BinarchtFaultInfo();
    }

}

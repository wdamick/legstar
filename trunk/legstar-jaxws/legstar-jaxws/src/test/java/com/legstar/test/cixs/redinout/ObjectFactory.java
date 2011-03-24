package com.legstar.test.cixs.redinout;

import javax.xml.bind.annotation.XmlRegistry;

/**
 * A JAXB ObjectFactory for wrapper classes.
 *
 */
@XmlRegistry
public class ObjectFactory {


    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes
     * for package: com.legstar.test.cixs.redinout.
     * 
     */
    public ObjectFactory() {
    }

    /**
     * @return an instance of {@link RedinoutHostHeader }
     * 
     */
    public RedinoutHostHeader createRedinoutHostHeader() {
        return new RedinoutHostHeader();
    }

    /**
     * @return an instance of {@link RedinoutRequest }
     * 
     */
    public RedinoutRequest createRedinoutRequest() {
        return new RedinoutRequest();
    }

    /**
     * @return an instance of {@link RedinoutResponse }
     * 
     */
    public RedinoutResponse createRedinoutResponse() {
        return new RedinoutResponse();
    }


    /**
     * @return an instance of {@link RedinoutFaultInfo }
     * 
     */
    public RedinoutFaultInfo createRedinoutFaultInfo() {
        return new RedinoutFaultInfo();
    }

}

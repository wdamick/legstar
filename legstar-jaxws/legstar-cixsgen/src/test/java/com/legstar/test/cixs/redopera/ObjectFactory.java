package com.legstar.test.cixs.redopera;

import javax.xml.bind.annotation.XmlRegistry;

/**
 * A JAXB ObjectFactory for wrapper classes.
 *
 */
@XmlRegistry
public class ObjectFactory {


    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes
     * for package: com.legstar.test.cixs.redopera.
     * 
     */
    public ObjectFactory() {
    }

    /**
     * @return an instance of {@link RedoperaHostHeader }
     * 
     */
    public RedoperaHostHeader createRedoperaHostHeader() {
        return new RedoperaHostHeader();
    }

    /**
     * @return an instance of {@link RedoperaRequest }
     * 
     */
    public RedoperaRequest createRedoperaRequest() {
        return new RedoperaRequest();
    }

    /**
     * @return an instance of {@link RedoperaResponse }
     * 
     */
    public RedoperaResponse createRedoperaResponse() {
        return new RedoperaResponse();
    }


    /**
     * @return an instance of {@link RedoperaFaultInfo }
     * 
     */
    public RedoperaFaultInfo createRedoperaFaultInfo() {
        return new RedoperaFaultInfo();
    }

}

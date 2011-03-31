package com.legstar.test.cixs.redbotha;

import javax.xml.bind.annotation.XmlRegistry;

/**
 * A JAXB ObjectFactory for wrapper classes.
 *
 */
@XmlRegistry
public class ObjectFactory {


    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes
     * for package: com.legstar.test.cixs.redbotha.
     * 
     */
    public ObjectFactory() {
    }

    /**
     * @return an instance of {@link RedbothaHostHeader }
     * 
     */
    public RedbothaHostHeader createRedbothaHostHeader() {
        return new RedbothaHostHeader();
    }

    /**
     * @return an instance of {@link RedbothaRequest }
     * 
     */
    public RedbothaRequest createRedbothaRequest() {
        return new RedbothaRequest();
    }

    /**
     * @return an instance of {@link RedbothaResponse }
     * 
     */
    public RedbothaResponse createRedbothaResponse() {
        return new RedbothaResponse();
    }


    /**
     * @return an instance of {@link RedbothaFaultInfo }
     * 
     */
    public RedbothaFaultInfo createRedbothaFaultInfo() {
        return new RedbothaFaultInfo();
    }

}

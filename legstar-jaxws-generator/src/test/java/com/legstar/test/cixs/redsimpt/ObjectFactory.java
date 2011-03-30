package com.legstar.test.cixs.redsimpt;

import javax.xml.bind.annotation.XmlRegistry;

/**
 * A JAXB ObjectFactory for wrapper classes.
 *
 */
@XmlRegistry
public class ObjectFactory {


    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes
     * for package: com.legstar.test.cixs.redsimpt.
     * 
     */
    public ObjectFactory() {
    }

    /**
     * @return an instance of {@link RedsimptHostHeader }
     * 
     */
    public RedsimptHostHeader createRedsimptHostHeader() {
        return new RedsimptHostHeader();
    }

    /**
     * @return an instance of {@link RedsimptRequest }
     * 
     */
    public RedsimptRequest createRedsimptRequest() {
        return new RedsimptRequest();
    }

    /**
     * @return an instance of {@link RedsimptResponse }
     * 
     */
    public RedsimptResponse createRedsimptResponse() {
        return new RedsimptResponse();
    }


    /**
     * @return an instance of {@link RedsimptFaultInfo }
     * 
     */
    public RedsimptFaultInfo createRedsimptFaultInfo() {
        return new RedsimptFaultInfo();
    }

}

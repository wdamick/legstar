package com.legstar.test.cixs.fixarsim;

import javax.xml.bind.annotation.XmlRegistry;

/**
 * A JAXB ObjectFactory for wrapper classes.
 *
 */
@XmlRegistry
public class ObjectFactory {


    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes
     * for package: com.legstar.test.cixs.fixarsim.
     * 
     */
    public ObjectFactory() {
    }

    /**
     * @return an instance of {@link FixarsimHostHeader }
     * 
     */
    public FixarsimHostHeader createFixarsimHostHeader() {
        return new FixarsimHostHeader();
    }

    /**
     * @return an instance of {@link FixarsimRequest }
     * 
     */
    public FixarsimRequest createFixarsimRequest() {
        return new FixarsimRequest();
    }

    /**
     * @return an instance of {@link FixarsimResponse }
     * 
     */
    public FixarsimResponse createFixarsimResponse() {
        return new FixarsimResponse();
    }


    /**
     * @return an instance of {@link FixarsimFaultInfo }
     * 
     */
    public FixarsimFaultInfo createFixarsimFaultInfo() {
        return new FixarsimFaultInfo();
    }

}

package com.legstar.test.cixs.arraysdo;

import javax.xml.bind.annotation.XmlRegistry;

/**
 * A JAXB ObjectFactory for wrapper classes.
 *
 */
@XmlRegistry
public class ObjectFactory {


    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes
     * for package: com.legstar.test.cixs.arraysdo.
     * 
     */
    public ObjectFactory() {
    }

    /**
     * @return an instance of {@link ArraysdoHostHeader }
     * 
     */
    public ArraysdoHostHeader createArraysdoHostHeader() {
        return new ArraysdoHostHeader();
    }

    /**
     * @return an instance of {@link ArraysdoRequest }
     * 
     */
    public ArraysdoRequest createArraysdoRequest() {
        return new ArraysdoRequest();
    }

    /**
     * @return an instance of {@link ArraysdoResponse }
     * 
     */
    public ArraysdoResponse createArraysdoResponse() {
        return new ArraysdoResponse();
    }


    /**
     * @return an instance of {@link ArraysdoFaultInfo }
     * 
     */
    public ArraysdoFaultInfo createArraysdoFaultInfo() {
        return new ArraysdoFaultInfo();
    }

}

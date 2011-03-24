package com.legstar.test.cixs.arraygrp;

import javax.xml.bind.annotation.XmlRegistry;

/**
 * A JAXB ObjectFactory for wrapper classes.
 *
 */
@XmlRegistry
public class ObjectFactory {


    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes
     * for package: com.legstar.test.cixs.arraygrp.
     * 
     */
    public ObjectFactory() {
    }

    /**
     * @return an instance of {@link ArraygrpHostHeader }
     * 
     */
    public ArraygrpHostHeader createArraygrpHostHeader() {
        return new ArraygrpHostHeader();
    }

    /**
     * @return an instance of {@link ArraygrpRequest }
     * 
     */
    public ArraygrpRequest createArraygrpRequest() {
        return new ArraygrpRequest();
    }

    /**
     * @return an instance of {@link ArraygrpResponse }
     * 
     */
    public ArraygrpResponse createArraygrpResponse() {
        return new ArraygrpResponse();
    }


    /**
     * @return an instance of {@link ArraygrpFaultInfo }
     * 
     */
    public ArraygrpFaultInfo createArraygrpFaultInfo() {
        return new ArraygrpFaultInfo();
    }

}

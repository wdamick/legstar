package com.legstar.test.cixs.arrayssm;

import javax.xml.bind.annotation.XmlRegistry;

/**
 * A JAXB ObjectFactory for wrapper classes.
 *
 */
@XmlRegistry
public class ObjectFactory {


    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes
     * for package: com.legstar.test.cixs.arrayssm.
     * 
     */
    public ObjectFactory() {
    }

    /**
     * @return an instance of {@link ArrayssmHostHeader }
     * 
     */
    public ArrayssmHostHeader createArrayssmHostHeader() {
        return new ArrayssmHostHeader();
    }

    /**
     * @return an instance of {@link ArrayssmRequest }
     * 
     */
    public ArrayssmRequest createArrayssmRequest() {
        return new ArrayssmRequest();
    }

    /**
     * @return an instance of {@link ArrayssmResponse }
     * 
     */
    public ArrayssmResponse createArrayssmResponse() {
        return new ArrayssmResponse();
    }


    /**
     * @return an instance of {@link ArrayssmFaultInfo }
     * 
     */
    public ArrayssmFaultInfo createArrayssmFaultInfo() {
        return new ArrayssmFaultInfo();
    }

}

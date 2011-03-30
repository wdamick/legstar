package com.legstar.test.cixs.binnatus;

import javax.xml.bind.annotation.XmlRegistry;

/**
 * A JAXB ObjectFactory for wrapper classes.
 *
 */
@XmlRegistry
public class ObjectFactory {


    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes
     * for package: com.legstar.test.cixs.binnatus.
     * 
     */
    public ObjectFactory() {
    }

    /**
     * @return an instance of {@link BinnatusHostHeader }
     * 
     */
    public BinnatusHostHeader createBinnatusHostHeader() {
        return new BinnatusHostHeader();
    }

    /**
     * @return an instance of {@link BinnatusRequest }
     * 
     */
    public BinnatusRequest createBinnatusRequest() {
        return new BinnatusRequest();
    }

    /**
     * @return an instance of {@link BinnatusResponse }
     * 
     */
    public BinnatusResponse createBinnatusResponse() {
        return new BinnatusResponse();
    }


    /**
     * @return an instance of {@link BinnatusFaultInfo }
     * 
     */
    public BinnatusFaultInfo createBinnatusFaultInfo() {
        return new BinnatusFaultInfo();
    }

}

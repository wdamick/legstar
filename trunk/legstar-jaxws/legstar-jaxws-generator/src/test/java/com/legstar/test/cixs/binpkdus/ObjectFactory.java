package com.legstar.test.cixs.binpkdus;

import javax.xml.bind.annotation.XmlRegistry;

/**
 * A JAXB ObjectFactory for wrapper classes.
 *
 */
@XmlRegistry
public class ObjectFactory {


    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes
     * for package: com.legstar.test.cixs.binpkdus.
     * 
     */
    public ObjectFactory() {
    }

    /**
     * @return an instance of {@link BinpkdusHostHeader }
     * 
     */
    public BinpkdusHostHeader createBinpkdusHostHeader() {
        return new BinpkdusHostHeader();
    }

    /**
     * @return an instance of {@link BinpkdusRequest }
     * 
     */
    public BinpkdusRequest createBinpkdusRequest() {
        return new BinpkdusRequest();
    }

    /**
     * @return an instance of {@link BinpkdusResponse }
     * 
     */
    public BinpkdusResponse createBinpkdusResponse() {
        return new BinpkdusResponse();
    }


    /**
     * @return an instance of {@link BinpkdusFaultInfo }
     * 
     */
    public BinpkdusFaultInfo createBinpkdusFaultInfo() {
        return new BinpkdusFaultInfo();
    }

}

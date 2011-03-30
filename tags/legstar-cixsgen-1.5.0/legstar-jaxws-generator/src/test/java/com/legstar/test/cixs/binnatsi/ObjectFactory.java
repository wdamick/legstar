package com.legstar.test.cixs.binnatsi;

import javax.xml.bind.annotation.XmlRegistry;

/**
 * A JAXB ObjectFactory for wrapper classes.
 *
 */
@XmlRegistry
public class ObjectFactory {


    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes
     * for package: com.legstar.test.cixs.binnatsi.
     * 
     */
    public ObjectFactory() {
    }

    /**
     * @return an instance of {@link BinnatsiHostHeader }
     * 
     */
    public BinnatsiHostHeader createBinnatsiHostHeader() {
        return new BinnatsiHostHeader();
    }

    /**
     * @return an instance of {@link BinnatsiRequest }
     * 
     */
    public BinnatsiRequest createBinnatsiRequest() {
        return new BinnatsiRequest();
    }

    /**
     * @return an instance of {@link BinnatsiResponse }
     * 
     */
    public BinnatsiResponse createBinnatsiResponse() {
        return new BinnatsiResponse();
    }


    /**
     * @return an instance of {@link BinnatsiFaultInfo }
     * 
     */
    public BinnatsiFaultInfo createBinnatsiFaultInfo() {
        return new BinnatsiFaultInfo();
    }

}

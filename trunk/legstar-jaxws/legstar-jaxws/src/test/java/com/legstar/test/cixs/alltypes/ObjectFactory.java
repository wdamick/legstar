package com.legstar.test.cixs.alltypes;

import javax.xml.bind.annotation.XmlRegistry;

/**
 * A JAXB ObjectFactory for wrapper classes.
 *
 */
@XmlRegistry
public class ObjectFactory {


    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes
     * for package: com.legstar.test.cixs.alltypes.
     * 
     */
    public ObjectFactory() {
    }

    /**
     * @return an instance of {@link AlltypesHostHeader }
     * 
     */
    public AlltypesHostHeader createAlltypesHostHeader() {
        return new AlltypesHostHeader();
    }

    /**
     * @return an instance of {@link AlltypesRequest }
     * 
     */
    public AlltypesRequest createAlltypesRequest() {
        return new AlltypesRequest();
    }

    /**
     * @return an instance of {@link AlltypesResponse }
     * 
     */
    public AlltypesResponse createAlltypesResponse() {
        return new AlltypesResponse();
    }


    /**
     * @return an instance of {@link AlltypesFaultInfo }
     * 
     */
    public AlltypesFaultInfo createAlltypesFaultInfo() {
        return new AlltypesFaultInfo();
    }

}

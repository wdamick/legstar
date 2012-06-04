package com.legstar.test.cixs.dplarcht;

import javax.xml.bind.annotation.XmlRegistry;

/**
 * A JAXB ObjectFactory for wrapper classes.
 *
 */
@XmlRegistry
public class ObjectFactory {


    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes
     * for package: com.legstar.test.cixs.dplarcht.
     * 
     */
    public ObjectFactory() {
    }

    /**
     * @return an instance of {@link DplarchtHostHeader }
     * 
     */
    public DplarchtHostHeader createDplarchtHostHeader() {
        return new DplarchtHostHeader();
    }

    /**
     * @return an instance of {@link DplarchtRequest }
     * 
     */
    public DplarchtRequest createDplarchtRequest() {
        return new DplarchtRequest();
    }

    /**
     * @return an instance of {@link DplarchtResponse }
     * 
     */
    public DplarchtResponse createDplarchtResponse() {
        return new DplarchtResponse();
    }


    /**
     * @return an instance of {@link DplarchtFaultInfo }
     * 
     */
    public DplarchtFaultInfo createDplarchtFaultInfo() {
        return new DplarchtFaultInfo();
    }

}

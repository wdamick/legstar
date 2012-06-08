package com.legstar.test.cixs.vararcom;

import javax.xml.bind.annotation.XmlRegistry;

/**
 * A JAXB ObjectFactory for wrapper classes.
 *
 */
@XmlRegistry
public class ObjectFactory {


    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes
     * for package: com.legstar.test.cixs.vararcom.
     * 
     */
    public ObjectFactory() {
    }

    /**
     * @return an instance of {@link VararcomHostHeader }
     * 
     */
    public VararcomHostHeader createVararcomHostHeader() {
        return new VararcomHostHeader();
    }

    /**
     * @return an instance of {@link VararcomRequest }
     * 
     */
    public VararcomRequest createVararcomRequest() {
        return new VararcomRequest();
    }

    /**
     * @return an instance of {@link VararcomResponse }
     * 
     */
    public VararcomResponse createVararcomResponse() {
        return new VararcomResponse();
    }


    /**
     * @return an instance of {@link VararcomFaultInfo }
     * 
     */
    public VararcomFaultInfo createVararcomFaultInfo() {
        return new VararcomFaultInfo();
    }

}

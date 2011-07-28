package com.legstar.test.cixs.redmulti;

import javax.xml.bind.annotation.XmlRegistry;

/**
 * A JAXB ObjectFactory for wrapper classes.
 *
 */
@XmlRegistry
public class ObjectFactory {


    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes
     * for package: com.legstar.test.cixs.redmulti.
     * 
     */
    public ObjectFactory() {
    }

    /**
     * @return an instance of {@link RedmultiHostHeader }
     * 
     */
    public RedmultiHostHeader createRedmultiHostHeader() {
        return new RedmultiHostHeader();
    }

    /**
     * @return an instance of {@link RedmultiRequest }
     * 
     */
    public RedmultiRequest createRedmultiRequest() {
        return new RedmultiRequest();
    }

    /**
     * @return an instance of {@link RedmultiResponse }
     * 
     */
    public RedmultiResponse createRedmultiResponse() {
        return new RedmultiResponse();
    }


    /**
     * @return an instance of {@link RedmultiFaultInfo }
     * 
     */
    public RedmultiFaultInfo createRedmultiFaultInfo() {
        return new RedmultiFaultInfo();
    }

}

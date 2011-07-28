package com.legstar.test.cixs.typesmix;

import javax.xml.bind.annotation.XmlRegistry;

/**
 * A JAXB ObjectFactory for wrapper classes.
 *
 */
@XmlRegistry
public class ObjectFactory {


    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes
     * for package: com.legstar.test.cixs.typesmix.
     * 
     */
    public ObjectFactory() {
    }

    /**
     * @return an instance of {@link TypesmixHostHeader }
     * 
     */
    public TypesmixHostHeader createTypesmixHostHeader() {
        return new TypesmixHostHeader();
    }

    /**
     * @return an instance of {@link TypesmixRequest }
     * 
     */
    public TypesmixRequest createTypesmixRequest() {
        return new TypesmixRequest();
    }

    /**
     * @return an instance of {@link TypesmixResponse }
     * 
     */
    public TypesmixResponse createTypesmixResponse() {
        return new TypesmixResponse();
    }


    /**
     * @return an instance of {@link TypesmixFaultInfo }
     * 
     */
    public TypesmixFaultInfo createTypesmixFaultInfo() {
        return new TypesmixFaultInfo();
    }

}

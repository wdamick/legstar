package com.legstar.test.cixs.floatmix;

import javax.xml.bind.annotation.XmlRegistry;

/**
 * A JAXB ObjectFactory for wrapper classes.
 *
 */
@XmlRegistry
public class ObjectFactory {


    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes
     * for package: com.legstar.test.cixs.floatmix.
     * 
     */
    public ObjectFactory() {
    }

    /**
     * @return an instance of {@link FloatmixHostHeader }
     * 
     */
    public FloatmixHostHeader createFloatmixHostHeader() {
        return new FloatmixHostHeader();
    }

    /**
     * @return an instance of {@link FloatmixRequest }
     * 
     */
    public FloatmixRequest createFloatmixRequest() {
        return new FloatmixRequest();
    }

    /**
     * @return an instance of {@link FloatmixResponse }
     * 
     */
    public FloatmixResponse createFloatmixResponse() {
        return new FloatmixResponse();
    }


    /**
     * @return an instance of {@link FloatmixFaultInfo }
     * 
     */
    public FloatmixFaultInfo createFloatmixFaultInfo() {
        return new FloatmixFaultInfo();
    }

}

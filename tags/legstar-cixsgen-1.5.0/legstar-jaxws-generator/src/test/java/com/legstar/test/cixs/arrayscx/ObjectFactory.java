package com.legstar.test.cixs.arrayscx;

import javax.xml.bind.annotation.XmlRegistry;

/**
 * A JAXB ObjectFactory for wrapper classes.
 *
 */
@XmlRegistry
public class ObjectFactory {


    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes
     * for package: com.legstar.test.cixs.arrayscx.
     * 
     */
    public ObjectFactory() {
    }

    /**
     * @return an instance of {@link ArrayscxHostHeader }
     * 
     */
    public ArrayscxHostHeader createArrayscxHostHeader() {
        return new ArrayscxHostHeader();
    }

    /**
     * @return an instance of {@link ArrayscxRequest }
     * 
     */
    public ArrayscxRequest createArrayscxRequest() {
        return new ArrayscxRequest();
    }

    /**
     * @return an instance of {@link ArrayscxResponse }
     * 
     */
    public ArrayscxResponse createArrayscxResponse() {
        return new ArrayscxResponse();
    }


    /**
     * @return an instance of {@link ArrayscxFaultInfo }
     * 
     */
    public ArrayscxFaultInfo createArrayscxFaultInfo() {
        return new ArrayscxFaultInfo();
    }

}

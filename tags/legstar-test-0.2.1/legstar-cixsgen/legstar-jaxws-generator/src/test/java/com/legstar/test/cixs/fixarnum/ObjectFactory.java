package com.legstar.test.cixs.fixarnum;

import javax.xml.bind.annotation.XmlRegistry;

/**
 * A JAXB ObjectFactory for wrapper classes.
 *
 */
@XmlRegistry
public class ObjectFactory {


    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes
     * for package: com.legstar.test.cixs.fixarnum.
     * 
     */
    public ObjectFactory() {
    }

    /**
     * @return an instance of {@link FixarnumHostHeader }
     * 
     */
    public FixarnumHostHeader createFixarnumHostHeader() {
        return new FixarnumHostHeader();
    }

    /**
     * @return an instance of {@link FixarnumRequest }
     * 
     */
    public FixarnumRequest createFixarnumRequest() {
        return new FixarnumRequest();
    }

    /**
     * @return an instance of {@link FixarnumResponse }
     * 
     */
    public FixarnumResponse createFixarnumResponse() {
        return new FixarnumResponse();
    }


    /**
     * @return an instance of {@link FixarnumFaultInfo }
     * 
     */
    public FixarnumFaultInfo createFixarnumFaultInfo() {
        return new FixarnumFaultInfo();
    }

}

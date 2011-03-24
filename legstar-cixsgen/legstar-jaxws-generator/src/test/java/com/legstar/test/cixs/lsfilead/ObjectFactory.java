package com.legstar.test.cixs.lsfilead;

import javax.xml.bind.annotation.XmlRegistry;

/**
 * A JAXB ObjectFactory for wrapper classes.
 *
 */
@XmlRegistry
public class ObjectFactory {


    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes
     * for package: com.legstar.test.cixs.lsfilead.
     * 
     */
    public ObjectFactory() {
    }

    /**
     * @return an instance of {@link LsfileadHostHeader }
     * 
     */
    public LsfileadHostHeader createLsfileadHostHeader() {
        return new LsfileadHostHeader();
    }

    /**
     * @return an instance of {@link LsfileadRequest }
     * 
     */
    public LsfileadRequest createLsfileadRequest() {
        return new LsfileadRequest();
    }

    /**
     * @return an instance of {@link LsfileadResponse }
     * 
     */
    public LsfileadResponse createLsfileadResponse() {
        return new LsfileadResponse();
    }


    /**
     * @return an instance of {@link LsfileadFaultInfo }
     * 
     */
    public LsfileadFaultInfo createLsfileadFaultInfo() {
        return new LsfileadFaultInfo();
    }

}

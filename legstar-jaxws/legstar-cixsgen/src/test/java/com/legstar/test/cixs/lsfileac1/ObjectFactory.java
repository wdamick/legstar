package com.legstar.test.cixs.lsfileac1;

import javax.xml.bind.annotation.XmlRegistry;

/**
 * A JAXB ObjectFactory for wrapper classes.
 *
 */
@XmlRegistry
public class ObjectFactory {


    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes
     * for package: com.legstar.test.cixs.lsfileac1.
     * 
     */
    public ObjectFactory() {
    }

    /**
     * @return an instance of {@link Lsfileac1HostHeader }
     * 
     */
    public Lsfileac1HostHeader createLsfileac1HostHeader() {
        return new Lsfileac1HostHeader();
    }

    /**
     * @return an instance of {@link LsfileacRequest }
     * 
     */
    public LsfileacRequest createLsfileacRequest() {
        return new LsfileacRequest();
    }

    /**
     * @return an instance of {@link LsfileacResponse }
     * 
     */
    public LsfileacResponse createLsfileacResponse() {
        return new LsfileacResponse();
    }


    /**
     * @return an instance of {@link LsfileacFaultInfo }
     * 
     */
    public LsfileacFaultInfo createLsfileacFaultInfo() {
        return new LsfileacFaultInfo();
    }

}

package com.legstar.test.cixs.varar021;

import javax.xml.bind.annotation.XmlRegistry;

/**
 * A JAXB ObjectFactory for wrapper classes.
 *
 */
@XmlRegistry
public class ObjectFactory {


    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes
     * for package: com.legstar.test.cixs.varar021.
     * 
     */
    public ObjectFactory() {
    }

    /**
     * @return an instance of {@link Varar021HostHeader }
     * 
     */
    public Varar021HostHeader createVarar021HostHeader() {
        return new Varar021HostHeader();
    }

    /**
     * @return an instance of {@link Varar021Request }
     * 
     */
    public Varar021Request createVarar021Request() {
        return new Varar021Request();
    }

    /**
     * @return an instance of {@link Varar021Response }
     * 
     */
    public Varar021Response createVarar021Response() {
        return new Varar021Response();
    }


    /**
     * @return an instance of {@link Varar021FaultInfo }
     * 
     */
    public Varar021FaultInfo createVarar021FaultInfo() {
        return new Varar021FaultInfo();
    }

}

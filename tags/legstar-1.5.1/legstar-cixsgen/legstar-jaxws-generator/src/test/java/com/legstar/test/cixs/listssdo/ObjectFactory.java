package com.legstar.test.cixs.listssdo;

import javax.xml.bind.annotation.XmlRegistry;

/**
 * A JAXB ObjectFactory for wrapper classes.
 *
 */
@XmlRegistry
public class ObjectFactory {


    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes
     * for package: com.legstar.test.cixs.listssdo.
     * 
     */
    public ObjectFactory() {
    }

    /**
     * @return an instance of {@link ListssdoHostHeader }
     * 
     */
    public ListssdoHostHeader createListssdoHostHeader() {
        return new ListssdoHostHeader();
    }

    /**
     * @return an instance of {@link ListssdoRequest }
     * 
     */
    public ListssdoRequest createListssdoRequest() {
        return new ListssdoRequest();
    }

    /**
     * @return an instance of {@link ListssdoResponse }
     * 
     */
    public ListssdoResponse createListssdoResponse() {
        return new ListssdoResponse();
    }


    /**
     * @return an instance of {@link ListssdoFaultInfo }
     * 
     */
    public ListssdoFaultInfo createListssdoFaultInfo() {
        return new ListssdoFaultInfo();
    }

}

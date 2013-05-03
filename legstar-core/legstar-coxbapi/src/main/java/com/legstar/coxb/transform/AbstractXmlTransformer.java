package com.legstar.coxb.transform;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;

public class AbstractXmlTransformer {

    /**
     * To reduce cost of instantiation for JAXBContext, and assuming a thread
     * safe implementation of JAXBContext (that of JDK 1.6 is), this provides
     * reusability within a VM.
     */
    private static Map < String, JAXBContext > JAXB_CONTEXTS = new ConcurrentHashMap < String, JAXBContext >();

    public static JAXBContext getJAXBContext(Class < ? > jaxbType)
            throws JAXBException {
        JAXBContext jaxbContext = JAXB_CONTEXTS.get(jaxbType.getName());
        if (jaxbContext == null) {
            jaxbContext = JAXBContext.newInstance(jaxbType);
            JAXB_CONTEXTS.put(jaxbType.getName(), jaxbContext);
        }
        return jaxbContext;
    }
}

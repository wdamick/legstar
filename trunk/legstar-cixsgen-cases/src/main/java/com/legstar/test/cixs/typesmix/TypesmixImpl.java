
package com.legstar.test.cixs.typesmix;
import javax.jws.WebService;

import javax.naming.Context;
import javax.naming.InitialContext;
import javax.naming.NamingException;

import com.legstar.host.invoke.HostInvoker;
import com.legstar.host.invoke.HostInvokerException;
import com.legstar.host.invoke.HostInvokerFactory;
import com.legstar.messaging.Address;

import com.legstar.test.coxb.typesmix.DfhcommareaType;

import com.legstar.test.coxb.typesmix.bind.DfhcommareaTypeBinding;


/**
 * Web service enpoint implementation.
 * 
 * This class was generated by CIXS generator.
 * 2007-07-09T13:01:28.031+02:00
 */

@WebService(endpointInterface = "com.legstar.test.cixs.typesmix.Typesmix",
        serviceName = "typesmixService",
        targetNamespace = "http://cixs.test.legstar.com/typesmix")
public class TypesmixImpl implements Typesmix {

    /** The JNDI locator for the configuration file name.*/
    private static final String JNDI_CONFIG_FILE =
        "java:comp/env/legstar/configFileName";
    
    /** The default configuration file name if not recovered from JNDI. */
    private static final String DEFAULT_CONFIG_FILE = "config.xml";

    /** The configuration file name. */
    private String mConfigFileName;


    /** Properties for operation typesmix. */
    private static final String  TYPESMIX_PROP_FILE = "typesmix.properties";

    /** {@inheritDoc} */
    public final DfhcommareaType typesmix(
            final DfhcommareaType request,
            final TypesmixHostHeader hostHeader)
            throws TypesmixFault {
    
        DfhcommareaType reply = null;
    
        try {
              
            /* Initialize invoker with static data and data from headers */
            HostInvoker mInvoker = HostInvokerFactory.createHostInvoker(
                mConfigFileName, getAddress(hostHeader), TYPESMIX_PROP_FILE);

            /* Prepare the input parameter set using static binding */
            DfhcommareaTypeBinding inputDfhcommarea =
                  new DfhcommareaTypeBinding(request);
            
            /* Prepare the output parameter set using static binding */
            DfhcommareaTypeBinding outputDfhcommarea =
                  new DfhcommareaTypeBinding();
            
            /* Call remote program */
            mInvoker.invoke((hostHeader == null) ? "typesmix" 
              : hostHeader.getHostRequestID(),
               inputDfhcommarea,
               outputDfhcommarea);

            
            /* Get reply object */
            reply = outputDfhcommarea.getDfhcommareaType(); 
                

        } catch (HostInvokerException e) {
            reportTypesmixFaultException(e,
              "Failed to invoke host program:");
        }

        return reply;
    }

    /**
    * Formats a fault element to notify client of an exception.
    *
    * @param e the exception which occured
    * @param text short message describing the context
    * @throws TypesmixFault the fault exception
    */
    private void reportTypesmixFaultException(
        final Exception e,
        final String text) throws TypesmixFault {

        TypesmixFaultInfo faultInfo = new TypesmixFaultInfo();
        faultInfo.setMessage(e.getMessage());
        faultInfo.setDetail("Operation="
                + "Typesmix"
                + " Package="
                + "com.legstar.test.cixs.typesmix");
        throw (new TypesmixFault(text + ' ' 
                + faultInfo.getMessage(), faultInfo));

    }


    /** Lookup the fonfiguration file name at construction time. */
    public TypesmixImpl() {
        try {
            Context initCtx = new InitialContext();
            mConfigFileName = (String) initCtx.lookup(JNDI_CONFIG_FILE);
        } catch (NamingException e) {
            mConfigFileName = DEFAULT_CONFIG_FILE;
        }
    }

    /**
    * Extracts header data from SOAP header and create an Address.
    * @param hostHeader the JAXB object mapping the SOAP header element
    * @return the new host Address
    */
    private Address getAddress(
        final TypesmixHostHeader hostHeader) {
        if (hostHeader == null) {
            return null;
        }
        Address address = new Address(hostHeader.getHostEndPoint());
        address.setHostCharset(hostHeader.getHostCharset());
        address.setHostUserID(hostHeader.getHostUserID());
        address.setHostPassword(hostHeader.getHostPassword());
        address.setHostTraceMode(hostHeader.getHostTraceMode());
        return address;
    }

}

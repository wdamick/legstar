package com.legstar.test.cixs.listssdo;
import java.rmi.server.UID;
import javax.jws.WebService;

import com.legstar.coxb.transform.HostTransformException;
import com.legstar.host.invoke.AbstractServiceAdapter;
import com.legstar.host.invoke.HostInvokerException;
import com.legstar.messaging.LegStarAddress;
import com.legstar.test.coxb.listssdo.Dfhcommarea;

/**
 * JAX-WS Service Adapter implementation.
 * Each method maps to a CICS program. 
 * 
 * This class was generated by LegStar Mainframe Web Service adapter generator.
 */
@WebService(endpointInterface = "com.legstar.test.cixs.listssdo.Listssdo",
            serviceName = "listssdoService",
            portName = "listssdoPort",
            targetNamespace = "http://cixs.test.legstar.com/listssdo")
public class ListssdoImpl extends AbstractServiceAdapter implements Listssdo {

    /** Name of this service adapter implementation. */
    private static final String  SERVICE_ADAPTER_NAME = "listssdo";

    /** Invoker implementation for operation listssdo. */
    private ListssdoProgramInvoker mListssdoProgramInvoker;

    /** Contructor creates a set of operation invokers. */
    public ListssdoImpl() {
        super(SERVICE_ADAPTER_NAME);
        mListssdoProgramInvoker = new ListssdoProgramInvoker(getConfigFileName());
    }
    
    /** {@inheritDoc} */
    public Dfhcommarea listssdo(
               final Dfhcommarea request,
               final ListssdoHostHeader hostHeader)
               throws ListssdoFault {
    
        try {
            return getListssdoProgramInvoker().listssdo(
                    getAddress(hostHeader), getRequestID(hostHeader), request);
        } catch (HostInvokerException e) {
            throw getListssdoFault(e, "Failed to invoke host program:");
        } catch (HostTransformException e) {
            throw getListssdoFault(e, "Failed to transform data:");
        }
    }

    /**
     * Formats a fault element to notify client of an exception.
     *
     * @param e the exception which occurred
     * @param text short message describing the context
     * @return the fault exception
     */
    public ListssdoFault getListssdoFault(
            final Exception e, final String text) {

        ListssdoFaultInfo faultInfo = new ListssdoFaultInfo();
        faultInfo.setMessage(e.getMessage());
        faultInfo.setDetail(getListssdoProgramInvoker().toString());
        return new ListssdoFault(text + ' ' 
                + faultInfo.getMessage(), faultInfo, e);
    }

        
    /**
     * Extracts header data from SOAP header and create a host address.
     * @param hostHeader the java object mapping the SOAP header element
     * @return the new host address
     */
    public LegStarAddress getAddress(
            final ListssdoHostHeader hostHeader) {
        if (hostHeader == null) {
            return null;
        }
        LegStarAddress address = new LegStarAddress(hostHeader.getHostEndPoint());
        address.setHostCharset(hostHeader.getHostCharset());
        address.setHostUserID(hostHeader.getHostUserID());
        address.setHostPassword(hostHeader.getHostPassword());
        address.setHostTraceMode(hostHeader.getHostTraceMode());
        return address;
    }

    /**
     * Generates a unique ID for a request. This can be passed from the client
     * using the host header.
     * @param hostHeader the java object mapping the SOAP header element
     * @return  a unique request ID
     */
    public String getRequestID(final ListssdoHostHeader hostHeader) {
        if (hostHeader != null && hostHeader.getHostRequestID() != null) {
            return hostHeader.getHostRequestID();
        } else {
            return getServiceAdapterName() + ":" + new UID().toString();
        }
    }

    /**
     * @return the invoker implementation for operation listssdo
     */
    public ListssdoProgramInvoker getListssdoProgramInvoker() {
        return mListssdoProgramInvoker;
    }

    /**
     * @param programInvoker the invoker implementation for operation listssdo to set
     */
    public void setListssdoProgramInvoker(
            final ListssdoProgramInvoker programInvoker) {
        mListssdoProgramInvoker = programInvoker;
    }
    
}

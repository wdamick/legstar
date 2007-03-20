
package com.legstar.test.cixs.typesmix;
import javax.jws.WebService;
import com.legstar.cixs.coxb.CIXSParameter;
import com.legstar.cixs.coxb.ICIXSInvoker;
import com.legstar.cixs.coxb.CIXSInvokerFactory;
import com.legstar.cixs.coxb.CIXSHeader;
import com.legstar.cixs.coxb.CIXSException;


/**
 * Web service enpoint implementation.
 * 
 * This class was generated by CIXS version 1.0.
 * 2007-02-06T14:31:00.578+01:00
 */

@WebService(endpointInterface = "com.legstar.test.cixs.typesmix.Typesmix",
        serviceName = "typesmixService",
        targetNamespace = "http://cixs.test.legstar.com/typesmix")
public class TypesmixImpl implements Typesmix {

  /** The input parameter set for the invoke method. */
  private CIXSParameter mInParameter;
  
  /** The output parameter set for the invoke method. */
  private CIXSParameter mOutParameter;

  /** The invoker object. */
  private ICIXSInvoker mInvoker;


  /**
   * Contructor gets an invoker object.
   * @throws TypesmixFault if fails to get invoker 
   */
  public TypesmixImpl() throws TypesmixFault {
    CIXSInvokerFactory cf = new CIXSInvokerFactory();
    try {
        mInvoker = cf.createInvoker();
        mInParameter = mInvoker.createParameter();
        mOutParameter = mInvoker.createParameter();
    } catch (CIXSException e) {
        reportTypesmixFaultException(e, "Failed to create an invoker");
    }
  }
  
  /** {@inheritDoc} */
  public final com.legstar.test.coxb.typesmix.
          DfhcommareaType typesmix(
          final com.legstar.test.coxb.typesmix.
                DfhcommareaType request,
          final TypesmixHostHeader hostHeader)
      throws TypesmixFault {
    
    /* The JAXB input factory. */
    com.legstar.test.coxb.typesmix.ObjectFactory jaxbInFactory =
          new com.legstar.test.coxb.typesmix.ObjectFactory(); 
    
    /* The JAXB output factory. */
    com.legstar.test.coxb.typesmix.ObjectFactory jaxbOutFactory =
          new com.legstar.test.coxb.typesmix.ObjectFactory();  
    
    com.legstar.test.coxb.typesmix.
    DfhcommareaType reply = null;
    
    try {
              
      /* Initialize invoker with static data and data from headers */
      mInvoker.initialize(getHostParameters(hostHeader), "typesmix");

      /* Prepare the input parameter set using static binding */
      com.legstar.test.coxb.typesmix.bind.
        DfhcommareaTypeBinding cein =
          new com.legstar.test.coxb.typesmix.bind.
              DfhcommareaTypeBinding(jaxbInFactory, request);
      mInParameter.setComplexBinding(cein);
      
      /* Prepare the output parameter set using static binding */
      com.legstar.test.coxb.typesmix.bind.
      DfhcommareaTypeBinding ceout =
          new com.legstar.test.coxb.typesmix.bind.
              DfhcommareaTypeBinding(jaxbOutFactory);
      mOutParameter.setComplexBinding(ceout);
      
      /* Call remote program */
      mInvoker.invoke(mInParameter, mOutParameter);
      
      /* Get reply object */
      reply = ceout.getJaxbObject(); 
      
    } catch (CIXSException e) {
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
    e.printStackTrace();
    TypesmixFaultInfo faultInfo = new TypesmixFaultInfo();
    faultInfo.setMessage(e.getMessage());
    faultInfo.setDetail("Operation="
            + "Typesmix"
            + " Package="
            + "com.legstar.test.cixs.typesmix");
    throw (new TypesmixFault(text + ' ' 
            + faultInfo.getMessage(), faultInfo));
    
  }


  /**
   * Extracts header data from SOAP header.
   * 
   * @param hostHeader the JAXB object mapping the SOAP header element
   * @return the header data
   */
  private CIXSHeader getHostParameters(
          final TypesmixHostHeader hostHeader) {
    if (hostHeader == null) {
        return null;
    }
    CIXSHeader ch = new CIXSHeader();
    ch.setHostUser(hostHeader.getHostUser());
    ch.setHostPassword(hostHeader.getHostPassword());
    ch.setHostIPAddress(hostHeader.getHostIPAddress());
    ch.setHostIPPort(hostHeader.getHostIPPort());
    ch.setHostCICWPath(hostHeader.getHostCICWPath());
    return ch;
  }
  
}

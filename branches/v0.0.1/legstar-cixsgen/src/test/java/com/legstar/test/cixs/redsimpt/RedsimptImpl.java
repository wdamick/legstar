
package com.legstar.test.cixs.redsimpt;
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
 * 2007-02-06T14:30:58.937+01:00
 */

@WebService(endpointInterface = "com.legstar.test.cixs.redsimpt.Redsimpt",
        serviceName = "redsimptService",
        targetNamespace = "http://cixs.test.legstar.com/redsimpt")
public class RedsimptImpl implements Redsimpt {

  /** The input parameter set for the invoke method. */
  private CIXSParameter mInParameter;
  
  /** The output parameter set for the invoke method. */
  private CIXSParameter mOutParameter;

  /** The invoker object. */
  private ICIXSInvoker mInvoker;


  /**
   * Contructor gets an invoker object.
   * @throws RedsimptFault if fails to get invoker 
   */
  public RedsimptImpl() throws RedsimptFault {
    CIXSInvokerFactory cf = new CIXSInvokerFactory();
    try {
        mInvoker = cf.createInvoker();
        mInParameter = mInvoker.createParameter();
        mOutParameter = mInvoker.createParameter();
    } catch (CIXSException e) {
        reportRedsimptFaultException(e, "Failed to create an invoker");
    }
  }
  
  /** {@inheritDoc} */
  public final com.legstar.test.coxb.redsimpt.
          DfhcommareaType redsimpt(
          final com.legstar.test.coxb.redsimpt.
                DfhcommareaType request,
          final RedsimptHostHeader hostHeader)
      throws RedsimptFault {
    
    /* The JAXB input factory. */
    com.legstar.test.coxb.redsimpt.ObjectFactory jaxbInFactory =
          new com.legstar.test.coxb.redsimpt.ObjectFactory(); 
    
    /* The JAXB output factory. */
    com.legstar.test.coxb.redsimpt.ObjectFactory jaxbOutFactory =
          new com.legstar.test.coxb.redsimpt.ObjectFactory();  
    
    com.legstar.test.coxb.redsimpt.
    DfhcommareaType reply = null;
    
    try {
              
      /* Initialize invoker with static data and data from headers */
      mInvoker.initialize(getHostParameters(hostHeader), "redsimpt");

      /* Prepare the input parameter set using static binding */
      com.legstar.test.coxb.redsimpt.bind.
        DfhcommareaTypeBinding cein =
          new com.legstar.test.coxb.redsimpt.bind.
              DfhcommareaTypeBinding(jaxbInFactory, request);
      mInParameter.setComplexBinding(cein);
      
      /* Prepare the output parameter set using static binding */
      com.legstar.test.coxb.redsimpt.bind.
      DfhcommareaTypeBinding ceout =
          new com.legstar.test.coxb.redsimpt.bind.
              DfhcommareaTypeBinding(jaxbOutFactory);
      mOutParameter.setComplexBinding(ceout);
      
      /* Call remote program */
      mInvoker.invoke(mInParameter, mOutParameter);
      
      /* Get reply object */
      reply = ceout.getJaxbObject(); 
      
    } catch (CIXSException e) {
      reportRedsimptFaultException(e,
          "Failed to invoke host program:");
    }

    return reply;
  }

  /**
   * Formats a fault element to notify client of an exception.
   *
   * @param e the exception which occured
   * @param text short message describing the context
   * @throws RedsimptFault the fault exception
   */
  private void reportRedsimptFaultException(
      final Exception e,
      final String text) throws RedsimptFault {
    e.printStackTrace();
    RedsimptFaultInfo faultInfo = new RedsimptFaultInfo();
    faultInfo.setMessage(e.getMessage());
    faultInfo.setDetail("Operation="
            + "Redsimpt"
            + " Package="
            + "com.legstar.test.cixs.redsimpt");
    throw (new RedsimptFault(text + ' ' 
            + faultInfo.getMessage(), faultInfo));
    
  }


  /**
   * Extracts header data from SOAP header.
   * 
   * @param hostHeader the JAXB object mapping the SOAP header element
   * @return the header data
   */
  private CIXSHeader getHostParameters(
          final RedsimptHostHeader hostHeader) {
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
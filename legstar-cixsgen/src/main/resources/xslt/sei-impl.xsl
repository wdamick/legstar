<?xml version="1.0" encoding="UTF-8"?>
<!-- ===============================================================================================
   XSLT for SEI implementation Generation. The class performs the host marshalling
   calls the backend system and unmarshalls the reply.
 -->
<xsl:stylesheet version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="text" omit-xml-declaration="yes" indent="yes"/>
<xsl:template match="/"><xsl:apply-templates select="cixs-service" /></xsl:template>

<!-- Generate the service endpoint implementation class -->
<xsl:template match="cixs-service">

  <!-- Determine the endpoint implementation java source file name -->
  <xsl:variable name="implementation-class-name">
    <xsl:choose>
      <xsl:when test="string-length(endpoint-implementation) > 0"><xsl:value-of select="endpoint-implementation"/></xsl:when>
      <xsl:otherwise><xsl:value-of select="concat(upper-case(substring(service-name,1,1)),substring(service-name,2))"/>Impl</xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <xsl:variable name="target-dir">
    <xsl:value-of select="translate(service-endpoint-package,'.','/')"/>
  </xsl:variable>
  
  <!-- Generate the dynamically built java source file -->
  <xsl:result-document href="{$target-dir}/{$implementation-class-name}.java" method="text" omit-xml-declaration="yes" indent="yes">
    <xsl:call-template name="generate-header"/>
    <xsl:call-template name="generate-class">
      <xsl:with-param name="implementation-class-name"><xsl:value-of select="$implementation-class-name"/></xsl:with-param>
    </xsl:call-template>
  </xsl:result-document>

</xsl:template>

<!-- ===============================================================================================
   Generate the package and import code
 -->
<xsl:template name="generate-header">
package <xsl:value-of select="service-endpoint-package"/>;
import javax.jws.WebService;

import javax.naming.Context;
import javax.naming.InitialContext;
import javax.naming.NamingException;

import com.legstar.host.invoke.HostInvoker;
import com.legstar.host.invoke.HostInvokerException;
import com.legstar.host.invoke.HostInvokerFactory;
import com.legstar.messaging.Address;
<xsl:if test="count(cixs-operation/input) > 1 or count(cixs-operation/output) > 1">
import java.util.LinkedHashMap;
import java.util.Map;
import com.legstar.coxb.ICobolComplexBinding;
</xsl:if>
<xsl:for-each select="cixs-operation">

<!-- Import the operation wrapper objects packages -->
<xsl:if test="string-length(operation-package) > 0 and (operation-package != ../service-endpoint-package)">
import <xsl:value-of select="operation-package"/>.*;</xsl:if>
</xsl:for-each>

/**
 * Web service enpoint implementation.
 * 
 * This class was generated by CIXS generator.
 * <xsl:value-of  select="current-dateTime()"/>
 */
</xsl:template>

<!-- ===============================================================================================
   Generate the code of the java class that implements the SEI
 -->
<xsl:template name="generate-class">
<xsl:param name="implementation-class-name"/>
  <xsl:variable name="sei-class-name">
    <xsl:choose>
      <xsl:when test="string-length(endpoint-interface) > 0"><xsl:value-of select="endpoint-interface"/></xsl:when>
      <xsl:otherwise><xsl:value-of select="concat(upper-case(substring(service-name,1,1)),substring(service-name,2))"/></xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <xsl:variable name="wsdl-service-name">
    <xsl:choose>
      <xsl:when test="string-length(wsdl-service-name) > 0"><xsl:value-of select="wsdl-service-name"/></xsl:when>
      <xsl:otherwise><xsl:value-of select="service-name"/>Service</xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <xsl:variable name="hostheader-class-name">
    <xsl:choose>
      <xsl:when test="string-length(endpoint-interface) > 0"><xsl:value-of select="endpoint-interface"/>HostHeader</xsl:when>
      <xsl:otherwise><xsl:value-of select="concat(upper-case(substring(service-name,1,1)),substring(service-name,2))"/>HostHeader</xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
@WebService(endpointInterface = "<xsl:value-of select="service-endpoint-package"/>.<xsl:value-of select="$sei-class-name"/>",
        serviceName = "<xsl:value-of select="$wsdl-service-name"/>",
        targetNamespace = "<xsl:value-of select="service-targetnamespace"/>")
public class <xsl:value-of select="$implementation-class-name"/> implements <xsl:value-of select="$sei-class-name"/> {

    /** The JNDI locator for the configuration file name.*/
    private static final String JNDI_CONFIG_FILE =
        "java:comp/env/legstar/configFileName";
    
    /** The default configuration file name if not recovered from JNDI. */
    private static final String DEFAULT_CONFIG_FILE = "config.xml";

    /** The configuration file name. */
    private String mConfigFileName;

<xsl:for-each select="cixs-operation">

    <xsl:variable name="operation-class-name">
        <xsl:value-of select="concat(upper-case(substring(operation-name,1,1)),substring(operation-name,2))"/>
    </xsl:variable>
    <xsl:variable name="operation-package">
        <xsl:choose>
            <xsl:when test="string-length(operation-package) > 0"><xsl:value-of select="operation-package"/></xsl:when>
            <xsl:otherwise><xsl:value-of select="../service-endpoint-package"/></xsl:otherwise>
        </xsl:choose>
    </xsl:variable>
    <xsl:variable name="operation-namespace">
        <xsl:choose>
            <xsl:when test="string-length(operation-targetnamespace) > 0"><xsl:value-of select="operation-targetnamespace"/></xsl:when>
            <xsl:otherwise><xsl:value-of select="../service-targetnamespace"/></xsl:otherwise>
        </xsl:choose>
    </xsl:variable>
    <xsl:variable name="fault-type">
        <xsl:choose>
            <xsl:when test="string-length(fault-type) > 0"><xsl:value-of select="fault-type"/></xsl:when>
            <xsl:otherwise><xsl:value-of select="$operation-class-name"/>Fault</xsl:otherwise>
        </xsl:choose>
    </xsl:variable>
    <xsl:variable name="fault-info-type">
        <xsl:choose>
            <xsl:when test="string-length(fault-info-type) > 0"><xsl:value-of select="fault-info-type"/></xsl:when>
            <xsl:otherwise><xsl:value-of select="$operation-class-name"/>FaultInfo</xsl:otherwise>
        </xsl:choose>
    </xsl:variable>
    <xsl:variable name="multiple-input">
        <xsl:choose>
            <xsl:when test="count(input) > 1">true</xsl:when>
            <xsl:otherwise>false</xsl:otherwise>
        </xsl:choose>
    </xsl:variable>
    <xsl:variable name="multiple-output">
        <xsl:choose>
            <xsl:when test="count(output) > 1">true</xsl:when>
            <xsl:otherwise>false</xsl:otherwise>
        </xsl:choose>
    </xsl:variable>
    <xsl:variable name="request-holder-type">
        <xsl:choose>
            <xsl:when test="string-length(request-holder-type) > 0"><xsl:value-of select="request-holder-type"/></xsl:when>
            <xsl:otherwise>
                <xsl:choose>
                    <xsl:when test="$multiple-input = 'true'"><xsl:value-of select="$operation-class-name"/>RequestHolder</xsl:when>
                    <xsl:otherwise><xsl:value-of select="input/@jaxb-package"/>.<xsl:value-of select="input/@jaxb-type"/></xsl:otherwise>
                </xsl:choose>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:variable>
    <xsl:variable name="response-holder-type">
        <xsl:choose>
            <xsl:when test="string-length(response-holder-type) > 0"><xsl:value-of select="response-holder-type"/></xsl:when>
            <xsl:otherwise>
                <xsl:choose>
                    <xsl:when test="$multiple-output = 'true'"><xsl:value-of select="$operation-class-name"/>ResponseHolder</xsl:when>
                    <xsl:otherwise><xsl:value-of select="output/@jaxb-package"/>.<xsl:value-of select="output/@jaxb-type"/></xsl:otherwise>
                </xsl:choose>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:variable>
    /** Properties for operation <xsl:value-of select="operation-name"/>. */
    private static final String  <xsl:value-of select="upper-case(operation-name)"/>_PROP_FILE = "<xsl:value-of select="operation-name"/>.properties";

    /** {@inheritDoc} */
    public final <xsl:value-of select="$response-holder-type"/><xsl:text> </xsl:text><xsl:value-of select="operation-name"/>(
        final <xsl:value-of select="$request-holder-type"/> request,
        final <xsl:value-of select="$sei-class-name"/>HostHeader hostHeader)
        throws <xsl:value-of select="$fault-type"/> {
    
        <xsl:value-of select="$response-holder-type"/> reply = null;
    
        try {
              
            /* Initialize invoker with static data and data from headers */
            HostInvoker mInvoker = HostInvokerFactory.createHostInvoker(
                mConfigFileName, getAddress(hostHeader), <xsl:value-of select="upper-case(operation-name)"/>_PROP_FILE);

            /* Prepare the input parameter set using static binding */
            <xsl:for-each select="input">
            <xsl:value-of select="@jaxb-package"/>.bind.
                <xsl:value-of select="@jaxb-type"/>Binding <xsl:value-of select="@bind-field-name"/> =
                  new <xsl:value-of select="@jaxb-package"/>.bind.
                      <xsl:value-of select="@jaxb-type"/>Binding(
                        new <xsl:value-of select="@jaxb-package"/>.ObjectFactory(),
                        request<xsl:if test="$multiple-input = 'true'">.get<xsl:value-of select="@jaxb-property-name"/>()</xsl:if>);
            </xsl:for-each>
            /* Prepare the output parameter set using static binding */
            <xsl:for-each select="output">
            <xsl:value-of select="@jaxb-package"/>.bind.
                <xsl:value-of select="@jaxb-type"/>Binding <xsl:value-of select="@bind-field-name"/> =
                  new <xsl:value-of select="@jaxb-package"/>.bind.
                      <xsl:value-of select="@jaxb-type"/>Binding(
                        new <xsl:value-of select="@jaxb-package"/>.ObjectFactory());
            </xsl:for-each>
            <xsl:if test="$multiple-input = 'true'">
            /* Map input binding variables to containers */
            Map &lt; String, ICobolComplexBinding &gt; inParts =
            	  new LinkedHashMap &lt; String, ICobolComplexBinding &gt;(); 
            <xsl:for-each select="input">
            inParts.put("<xsl:value-of select="@cics-container"/>", <xsl:value-of select="@bind-field-name"/>);
            </xsl:for-each>
            </xsl:if>
            <xsl:if test="$multiple-output = 'true'">
            /* Map output binding variables to containers */
            Map &lt; String, ICobolComplexBinding &gt; outParts =
            	  new LinkedHashMap &lt; String, ICobolComplexBinding &gt;(); 
            <xsl:for-each select="output">
            outParts.put("<xsl:value-of select="@cics-container"/>", <xsl:value-of select="@bind-field-name"/>);
            </xsl:for-each>
            </xsl:if>
            /* Call remote program */
            mInvoker.invoke((hostHeader == null) ? "<xsl:value-of select="operation-name"/>" 
              : hostHeader.getHostRequestID(),
               <xsl:choose><xsl:when test="$multiple-input = 'true'">inParts</xsl:when><xsl:otherwise><xsl:value-of select="input/@bind-field-name"/></xsl:otherwise></xsl:choose>,
               <xsl:choose><xsl:when test="$multiple-output = 'true'">outParts</xsl:when><xsl:otherwise><xsl:value-of select="output/@bind-field-name"/></xsl:otherwise></xsl:choose>);

            <xsl:choose>
                <xsl:when test="$multiple-output = 'true'">
            /* Get reply objects */
            reply = new <xsl:value-of select="$response-holder-type"/>();
            <xsl:for-each select="output">
            reply.set<xsl:value-of select="@jaxb-property-name"/>(<xsl:value-of select="@bind-field-name"/>.getJaxbObject());
            </xsl:for-each>
                </xsl:when>
                <xsl:otherwise>
            /* Get reply object */
            reply = <xsl:value-of select="output/@bind-field-name"/>.getJaxbObject(); 
                </xsl:otherwise>
            </xsl:choose>

        } catch (HostInvokerException e) {
          report<xsl:value-of select="$fault-type"/>Exception(e,
              "Failed to invoke host program:");
        }

        return reply;
    }

    /**
    * Formats a fault element to notify client of an exception.
    *
    * @param e the exception which occured
    * @param text short message describing the context
    * @throws <xsl:value-of select="$fault-type"/> the fault exception
    */
    private void report<xsl:value-of select="$fault-type"/>Exception(
        final Exception e,
        final String text) throws <xsl:value-of select="$fault-type"/> {

        <xsl:value-of select="$fault-info-type"/> faultInfo = new <xsl:value-of select="$fault-info-type"/>();
        faultInfo.setMessage(e.getMessage());
        faultInfo.setDetail("Operation="
                + "<xsl:value-of select="$operation-class-name"/>"
                + " Package="
                + "<xsl:value-of select="$operation-package"/>");
        throw (new <xsl:value-of select="$fault-type"/>(text + ' ' 
                + faultInfo.getMessage(), faultInfo));

    }

</xsl:for-each>
    /** Lookup the fonfiguration file name at construction time. */
    public <xsl:value-of select="$implementation-class-name"/>() {
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
        final <xsl:value-of select="$hostheader-class-name"/> hostHeader) {
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
</xsl:template>

</xsl:stylesheet>

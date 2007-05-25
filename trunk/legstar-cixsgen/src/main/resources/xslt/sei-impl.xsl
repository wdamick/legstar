<?xml version="1.0" encoding="UTF-8"?>
<!-- ===============================================================================================
   XSLT for SEI implementation Generation. The class performs the host marshalling
   calls the backend system and unmarshalls the reply.
 -->
<xsl:stylesheet version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="text" omit-xml-declaration="yes" indent="yes"/>
<xsl:template match="/"><xsl:apply-templates select="cixsService" /></xsl:template>

<!-- Generate the service endpoint implementation class -->
<xsl:template match="cixsService">

  <!-- Determine the endpoint implementation java source file name -->
  <xsl:variable name="implementation-class-name">
    <xsl:choose>
      <xsl:when test="string-length(@endpointImplementationClassName) > 0"><xsl:value-of select="@endpointImplementationClassName"/></xsl:when>
      <xsl:otherwise><xsl:value-of select="concat(upper-case(substring(@name,1,1)),substring(@name,2))"/>Impl</xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <xsl:variable name="target-dir">
    <xsl:value-of select="translate(@endpointPackageName,'.','/')"/>
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
package <xsl:value-of select="@endpointPackageName"/>;
import javax.jws.WebService;

import javax.naming.Context;
import javax.naming.InitialContext;
import javax.naming.NamingException;

import com.legstar.host.invoke.HostInvoker;
import com.legstar.host.invoke.HostInvokerException;
import com.legstar.host.invoke.HostInvokerFactory;
import com.legstar.messaging.Address;
<xsl:if test="count(cixsOperation/input) > 1 or count(cixsOperation/output) > 1">
import java.util.LinkedHashMap;
import java.util.Map;
import com.legstar.coxb.ICobolComplexBinding;
</xsl:if>
<xsl:for-each select="cixsOperation">

<!-- Import the operation wrapper objects packages -->
<xsl:if test="string-length(@packageName) > 0 and (@packageName != ../@endpointPackageName)">
import <xsl:value-of select="@packageName"/>.*;</xsl:if>
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
      <xsl:when test="string-length(@endpointInterfaceClassName) > 0"><xsl:value-of select="@endpointInterfaceClassName"/></xsl:when>
      <xsl:otherwise><xsl:value-of select="concat(upper-case(substring(@name,1,1)),substring(@name,2))"/></xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <xsl:variable name="wsdl-service-name">
    <xsl:choose>
      <xsl:when test="string-length(@wsdlServiceName) > 0"><xsl:value-of select="@wsdlServiceName"/></xsl:when>
      <xsl:otherwise><xsl:value-of select="@name"/>Service</xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <xsl:variable name="hostheader-class-name"><xsl:value-of select="$sei-class-name"/>HostHeader</xsl:variable>
@WebService(endpointInterface = "<xsl:value-of select="@endpointPackageName"/>.<xsl:value-of select="$sei-class-name"/>",
        serviceName = "<xsl:value-of select="$wsdl-service-name"/>",
        targetNamespace = "<xsl:value-of select="@targetNamespace"/>")
public class <xsl:value-of select="$implementation-class-name"/> implements <xsl:value-of select="$sei-class-name"/> {

    /** The JNDI locator for the configuration file name.*/
    private static final String JNDI_CONFIG_FILE =
        "java:comp/env/legstar/configFileName";
    
    /** The default configuration file name if not recovered from JNDI. */
    private static final String DEFAULT_CONFIG_FILE = "config.xml";

    /** The configuration file name. */
    private String mConfigFileName;

<xsl:for-each select="cixsOperation">

    <xsl:variable name="operation-class-name">
        <xsl:value-of select="concat(upper-case(substring(@name,1,1)),substring(@name,2))"/>
    </xsl:variable>
    <xsl:variable name="operation-package-name">
        <xsl:choose>
            <xsl:when test="string-length(@packageName) > 0"><xsl:value-of select="@packageName"/></xsl:when>
            <xsl:otherwise><xsl:value-of select="../@endpointPackageName"/></xsl:otherwise>
        </xsl:choose>
    </xsl:variable>
    <xsl:variable name="fault-type">
        <xsl:choose>
            <xsl:when test="string-length(@faultType) > 0"><xsl:value-of select="@faultType"/></xsl:when>
            <xsl:otherwise><xsl:value-of select="$operation-class-name"/>Fault</xsl:otherwise>
        </xsl:choose>
    </xsl:variable>
    <xsl:variable name="fault-info-type">
        <xsl:choose>
            <xsl:when test="string-length(@faultInfoType) > 0"><xsl:value-of select="@faultInfoType"/></xsl:when>
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
            <xsl:when test="string-length(@requestHolderType) > 0"><xsl:value-of select="@requestHolderType"/></xsl:when>
            <xsl:otherwise>
                <xsl:choose>
                    <xsl:when test="$multiple-input = 'true'"><xsl:value-of select="$operation-class-name"/>RequestHolder</xsl:when>
                    <xsl:otherwise><xsl:value-of select="input/@jaxbPackageName"/>.<xsl:value-of select="input/@jaxbType"/></xsl:otherwise>
                </xsl:choose>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:variable>
    <xsl:variable name="response-holder-type">
        <xsl:choose>
            <xsl:when test="string-length(@responseHolderType) > 0"><xsl:value-of select="@responseHolderType"/></xsl:when>
            <xsl:otherwise>
                <xsl:choose>
                    <xsl:when test="$multiple-output = 'true'"><xsl:value-of select="$operation-class-name"/>ResponseHolder</xsl:when>
                    <xsl:otherwise><xsl:value-of select="output/@jaxbPackageName"/>.<xsl:value-of select="output/@jaxbType"/></xsl:otherwise>
                </xsl:choose>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:variable>
    /** Properties for operation <xsl:value-of select="@name"/>. */
    private static final String  <xsl:value-of select="upper-case(@name)"/>_PROP_FILE = "<xsl:value-of select="@name"/>.properties";

    /** {@inheritDoc} */
    public final <xsl:value-of select="$response-holder-type"/><xsl:text> </xsl:text><xsl:value-of select="@name"/>(
        final <xsl:value-of select="$request-holder-type"/> request,
        final <xsl:value-of select="$sei-class-name"/>HostHeader hostHeader)
        throws <xsl:value-of select="$fault-type"/> {
    
        <xsl:value-of select="$response-holder-type"/> reply = null;
    
        try {
              
            /* Initialize invoker with static data and data from headers */
            HostInvoker mInvoker = HostInvokerFactory.createHostInvoker(
                mConfigFileName, getAddress(hostHeader), <xsl:value-of select="upper-case(@name)"/>_PROP_FILE);

            /* Prepare the input parameter set using static binding */
            <xsl:for-each select="input">
            <xsl:value-of select="@jaxbPackageName"/>.bind.
                <xsl:value-of select="@jaxbType"/>Binding input<xsl:if test="count(../input) &gt; 1"><xsl:value-of select="position()"/></xsl:if><xsl:value-of select="@jaxbPropertyName"/> =
                  new <xsl:value-of select="@jaxbPackageName"/>.bind.
                      <xsl:value-of select="@jaxbType"/>Binding(
                        new <xsl:value-of select="@jaxbPackageName"/>.ObjectFactory(),
                        request<xsl:if test="$multiple-input = 'true'">.get<xsl:value-of select="@jaxbPropertyName"/>()</xsl:if>);
            </xsl:for-each>
            /* Prepare the output parameter set using static binding */
            <xsl:for-each select="output">
            <xsl:value-of select="@jaxbPackageName"/>.bind.
                <xsl:value-of select="@jaxbType"/>Binding output<xsl:if test="count(../output) &gt; 1"><xsl:value-of select="position()"/></xsl:if><xsl:value-of select="@jaxbPropertyName"/> =
                  new <xsl:value-of select="@jaxbPackageName"/>.bind.
                      <xsl:value-of select="@jaxbType"/>Binding(
                        new <xsl:value-of select="@jaxbPackageName"/>.ObjectFactory());
            </xsl:for-each>
            <xsl:if test="$multiple-input = 'true'">
            /* Map input binding variables to containers */
            Map &lt; String, ICobolComplexBinding &gt; inParts =
            	  new LinkedHashMap &lt; String, ICobolComplexBinding &gt;(); 
            <xsl:for-each select="input">
            inParts.put("<xsl:value-of select="@cicsContainer"/>", input<xsl:if test="count(../input) &gt; 1"><xsl:value-of select="position()"/></xsl:if><xsl:value-of select="@jaxbPropertyName"/>);
            </xsl:for-each>
            </xsl:if>
            <xsl:if test="$multiple-output = 'true'">
            /* Map output binding variables to containers */
            Map &lt; String, ICobolComplexBinding &gt; outParts =
            	  new LinkedHashMap &lt; String, ICobolComplexBinding &gt;(); 
            <xsl:for-each select="output">
            outParts.put("<xsl:value-of select="@cicsContainer"/>", output<xsl:if test="count(../output) &gt; 1"><xsl:value-of select="position()"/></xsl:if><xsl:value-of select="@jaxbPropertyName"/>);
            </xsl:for-each>
            </xsl:if>
            /* Call remote program */
            mInvoker.invoke((hostHeader == null) ? "<xsl:value-of select="@name"/>" 
              : hostHeader.getHostRequestID(),
               <xsl:choose><xsl:when test="$multiple-input = 'true'">inParts</xsl:when><xsl:otherwise>input<xsl:value-of select="input/@jaxbPropertyName"/></xsl:otherwise></xsl:choose>,
               <xsl:choose><xsl:when test="$multiple-output = 'true'">outParts</xsl:when><xsl:otherwise>output<xsl:value-of select="output/@jaxbPropertyName"/></xsl:otherwise></xsl:choose>);

            <xsl:choose>
                <xsl:when test="$multiple-output = 'true'">
            /* Get reply objects */
            reply = new <xsl:value-of select="$response-holder-type"/>();
            <xsl:for-each select="output">
            reply.set<xsl:value-of select="@jaxbPropertyName"/>(output<xsl:if test="count(../output) &gt; 1"><xsl:value-of select="position()"/></xsl:if><xsl:value-of select="@jaxbPropertyName"/>.getJaxbObject());
            </xsl:for-each>
                </xsl:when>
                <xsl:otherwise>
            /* Get reply object */
            reply = output<xsl:value-of select="output/@jaxbPropertyName"/>.getJaxbObject(); 
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
                + "<xsl:value-of select="$operation-package-name"/>");
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

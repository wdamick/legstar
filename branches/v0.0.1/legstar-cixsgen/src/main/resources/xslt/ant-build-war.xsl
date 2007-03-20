<?xml version="1.0" encoding="UTF-8"?>
<!-- ===============================================================================================
	 XSLT for a build-war.xml Generation. This ant script creates the WAR file to deploy the
	 Web Service to a target JAX-WS servlet engine.
 -->
<xsl:stylesheet version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="xml" omit-xml-declaration="no" indent="yes"/>
<xsl:param name="wdd-dir"/>
<xsl:param name="prop-dir"/>
<xsl:param name="war-dir"/>
<xsl:param name="jaxb-bin-dir"/>
<xsl:param name="cixs-bin-dir"/>
<xsl:param name="cust-bin-dir"/>
<xsl:template match="/"><xsl:apply-templates select="cixs-service"/></xsl:template>

<xsl:template match="cixs-service">
	<xsl:call-template name="generate-content"/>
</xsl:template>

<!-- ===============================================================================================
	 Generate the content of the build-war.xml file
 -->
<xsl:template name="generate-content">

	<!--  Provide default values for missing parameters -->
	<xsl:variable name="webapp-name">
		<xsl:choose>
			<xsl:when test="string-length(webapp-name) > 0"><xsl:value-of select="webapp-name"/></xsl:when>
			<xsl:otherwise>cixs-<xsl:value-of select="service-name"/></xsl:otherwise>
		</xsl:choose>
	</xsl:variable>

	<xsl:variable name="sei-class-name">
		<xsl:choose>
			<xsl:when test="string-length(endpoint-interface) > 0"><xsl:value-of select="endpoint-interface"/></xsl:when>
			<xsl:otherwise><xsl:value-of select="concat(upper-case(substring(service-name,1,1)),substring(service-name,2))"/></xsl:otherwise>
		</xsl:choose>
	</xsl:variable>

	<!--  Create content -->
	<xsl:element  name = "project">
    	<xsl:attribute  name = "basedir" >..</xsl:attribute>
	   	<xsl:attribute  name = "default" >create-war</xsl:attribute>
	   	<xsl:attribute  name = "name" >build-war</xsl:attribute>
		
		<xsl:element  name = "property">
	    	<xsl:attribute  name = "environment" >env</xsl:attribute>
		</xsl:element>
		<xsl:element  name = "property">
	    	<xsl:attribute  name = "name" >service</xsl:attribute>
	    	<xsl:attribute  name = "value" ><xsl:value-of select="service-name"/></xsl:attribute>
		</xsl:element>

		<xsl:element  name = "target">
	    	<xsl:attribute  name = "name" >clean</xsl:attribute>
			<xsl:element  name = "delete">
		    	<xsl:attribute  name = "file" ><xsl:value-of select="$war-dir"/>/<xsl:value-of select="$webapp-name"/>.war</xsl:attribute>
		    	<xsl:attribute  name = "includeEmptyDirs" >true</xsl:attribute>
		    	<xsl:attribute  name = "quiet" >true</xsl:attribute>
			</xsl:element>
		</xsl:element>
		<xsl:element  name = "target">
	    	<xsl:attribute  name = "name" >create-war</xsl:attribute>
	    	<xsl:attribute  name = "depends" >clean</xsl:attribute>
			<xsl:element  name = "war">
		    	<xsl:attribute  name = "warfile" ><xsl:value-of select="$war-dir"/>/<xsl:value-of select="$webapp-name"/>.war</xsl:attribute>
		    	<xsl:attribute  name = "webxml" ><xsl:value-of select="$wdd-dir"/>/web.xml</xsl:attribute>
				<xsl:element  name = "webinf">
			    	<xsl:attribute  name = "dir" ><xsl:value-of select="$wdd-dir"/></xsl:attribute>
			    	<xsl:attribute  name = "includes" >sun-jaxws.xml</xsl:attribute>
				</xsl:element>
				<xsl:for-each select="cixs-operation">
					<xsl:element  name = "classes">
				    	<xsl:attribute  name = "dir" ><xsl:value-of select="$jaxb-bin-dir"/></xsl:attribute>
						<xsl:element  name = "include">
				    		<xsl:attribute  name = "name" ><xsl:value-of select="input/@jaxb-classes-location"/>/**/*.class</xsl:attribute>
						</xsl:element>
					</xsl:element>
					<xsl:if test="not(input/@jaxb-package = output/@jaxb-package)">
						<xsl:element  name = "classes">
					    	<xsl:attribute  name = "dir" ><xsl:value-of select="$jaxb-bin-dir"/></xsl:attribute>
							<xsl:element  name = "include">
					    		<xsl:attribute  name = "name" ><xsl:value-of select="output/@jaxb-classes-location"/>/**/*.class</xsl:attribute>
							</xsl:element>
						</xsl:element>
					</xsl:if>
				</xsl:for-each>
				<xsl:element  name = "classes">
			    	<xsl:attribute  name = "dir" ><xsl:value-of select="$cixs-bin-dir"/></xsl:attribute>
					<xsl:element  name = "include">
			    		<xsl:attribute  name = "name" >**/${service}/**/*.class</xsl:attribute>
					</xsl:element>
				</xsl:element>
				<xsl:element  name = "classes">
			    	<xsl:attribute  name = "dir" ><xsl:value-of select="$cust-bin-dir"/></xsl:attribute>
					<xsl:element  name = "include">
			    		<xsl:attribute  name = "name" >**/${service}/**/*.class</xsl:attribute>
					</xsl:element>
				</xsl:element>
				<xsl:element  name = "classes">
			    	<xsl:attribute  name = "dir" ><xsl:value-of select="$prop-dir"/></xsl:attribute>
		    		<xsl:for-each select="cixs-operation">
						<xsl:element  name = "include">
				    		<xsl:attribute  name = "name" ><xsl:value-of select="operation-name"/>.properties</xsl:attribute>
						</xsl:element>
		    		</xsl:for-each>
				</xsl:element>
			</xsl:element>
		</xsl:element>
    </xsl:element>
</xsl:template>

</xsl:stylesheet>

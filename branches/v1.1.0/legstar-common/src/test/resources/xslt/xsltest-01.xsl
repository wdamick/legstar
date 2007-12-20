<?xml version="1.0" encoding="UTF-8"?>
<!-- ===============================================================================================
   XSLT for Type binding Generation. For each JAXB complex type with Cobol annotations, this 
   generates runtime binding code which is much faster than reflection and annotation APIs.
 -->
<xsl:stylesheet version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output indent="yes"/>
  <xsl:template match="/">
    <xsl:for-each-group select="CATALOG//CD" group-by="COUNTRY">
      <xsl:sort select="COUNTRY"/>
      <COUNTRY name="{COUNTRY}">
        <xsl:for-each-group select="current-group()" group-by="YEAR">
          <YEAR year="{YEAR}">
            <xsl:copy-of select="current-group()/TITLE"/>
          </YEAR>
        </xsl:for-each-group>
      </COUNTRY>
    </xsl:for-each-group>
  </xsl:template>
</xsl:stylesheet>



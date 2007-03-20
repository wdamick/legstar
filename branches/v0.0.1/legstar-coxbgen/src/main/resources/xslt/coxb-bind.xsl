<?xml version="1.0" encoding="UTF-8"?>
<!-- ===============================================================================================
   XSLT for Type binding Generation. For each JAXB complex type with Cobol annotations, this 
   generates runtime binding code which is much faster than reflection and annotation APIs.
 -->
<xsl:stylesheet version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="text" omit-xml-declaration="yes" indent="yes"/>

<xsl:param name="marshal-choice-strategy-classname-exists"/>
<xsl:param name="unmarshal-choice-strategy-classname-exists"/>

<xsl:template match="/">
    <xsl:apply-templates select="coxb-type" />
  <xsl:apply-templates select="coxb-type[string-length(@marshalChoiceStrategyClassName) > 0]" mode="choiceStrategy">
     <xsl:with-param name="choice-strategy-class-qual-name" select="coxb-type/@marshalChoiceStrategyClassName"/>
     <xsl:with-param name="choice-strategy-class-exists" select="$marshal-choice-strategy-classname-exists"/>
  </xsl:apply-templates>
  <xsl:apply-templates select="coxb-type[string-length(@unmarshalChoiceStrategyClassName) > 0]" mode="choiceStrategy">
     <xsl:with-param name="choice-strategy-class-qual-name" select="coxb-type/@unmarshalChoiceStrategyClassName"/>
     <xsl:with-param name="choice-strategy-class-exists" select="$unmarshal-choice-strategy-classname-exists"/>
  </xsl:apply-templates>
</xsl:template>

<!-- Generate the JAXB binding class -->
<xsl:template match="coxb-type">

  <!-- Determine the binding java source file name and location -->
  <xsl:variable name="binding-class-name">
    <xsl:value-of select="@name"/>
  </xsl:variable>
  <xsl:variable name="binding-type-package">
    <xsl:value-of select="concat(jaxb-type-package,'.bind')"/>
  </xsl:variable>
  <xsl:variable name="target-dir">
    <xsl:value-of select="translate($binding-type-package,'.','/')"/>
  </xsl:variable>
  
  <!-- Generate the dynamically built java source file -->
  <xsl:result-document href="{$target-dir}/{$binding-class-name}.java" method="text" omit-xml-declaration="yes" indent="yes">
    <xsl:call-template name="generate-header">
      <xsl:with-param name="binding-type-package"><xsl:value-of select="$binding-type-package"/></xsl:with-param>
    </xsl:call-template>
    <xsl:call-template name="generate-class">
      <xsl:with-param name="binding-class-name"><xsl:value-of select="$binding-class-name"/></xsl:with-param>
    </xsl:call-template>
  </xsl:result-document>

</xsl:template>

<!-- ===============================================================================================
   Generate the package and import code
 -->
<xsl:template name="generate-header">
<xsl:param name="binding-type-package"/>
package <xsl:value-of select="$binding-type-package"/>;

import com.legstar.host.HostException;
import com.legstar.coxb.ICobolBinding;
import com.legstar.coxb.rt.CComplexBinding;
<xsl:if test="count(coxb-property[@binding-type = 'CChoiceBinding']) > 0 or @type='choice'">
import com.legstar.coxb.rt.CChoiceBinding;</xsl:if>
<xsl:if test="count(coxb-property[@binding-type = 'CBinaryBinding']) > 0">
import com.legstar.coxb.rt.CBinaryBinding;</xsl:if>
<xsl:if test="count(coxb-property[@binding-type = 'CDoubleBinding']) > 0">
import com.legstar.coxb.rt.CDoubleBinding;</xsl:if>
<xsl:if test="count(coxb-property[@binding-type = 'CFloatBinding']) > 0">
import com.legstar.coxb.rt.CFloatBinding;</xsl:if>
<xsl:if test="count(coxb-property[@binding-type = 'COctetStreamBinding']) > 0">
import com.legstar.coxb.rt.COctetStreamBinding;</xsl:if>
<xsl:if test="count(coxb-property[@binding-type = 'CStringBinding']) > 0">
import com.legstar.coxb.rt.CStringBinding;</xsl:if>
<xsl:if test="count(coxb-property[@binding-type = 'CNationalBinding']) > 0">
import com.legstar.coxb.rt.CNationalBinding;</xsl:if>
<xsl:if test="count(coxb-property[@binding-type = 'CPackedDecimalBinding']) > 0">
import com.legstar.coxb.rt.CPackedDecimalBinding;</xsl:if>
<xsl:if test="count(coxb-property[@binding-type = 'CZonedDecimalBinding']) > 0">
import com.legstar.coxb.rt.CZonedDecimalBinding;</xsl:if>
<xsl:if test="count(coxb-property[@binding-type = 'CArrayBinaryBinding']) > 0">
import com.legstar.coxb.rt.CArrayBinaryBinding;</xsl:if>
<xsl:if test="count(coxb-property[@binding-type = 'CArrayDoubleBinding']) > 0">
import com.legstar.coxb.rt.CArrayDoubleBinding;</xsl:if>
<xsl:if test="count(coxb-property[@binding-type = 'CArrayFloatBinding']) > 0">
import com.legstar.coxb.rt.CArrayFloatBinding;</xsl:if>
<xsl:if test="count(coxb-property[@binding-type = 'CArrayOctetStreamBinding']) > 0">
import com.legstar.coxb.rt.CArrayOctetStreamBinding;</xsl:if>
<xsl:if test="count(coxb-property[@binding-type = 'CArrayStringBinding']) > 0">
import com.legstar.coxb.rt.CArrayStringBinding;</xsl:if>
<xsl:if test="count(coxb-property[@binding-type = 'CArrayNationalBinding']) > 0">
import com.legstar.coxb.rt.CArrayNationalBinding;</xsl:if>
<xsl:if test="count(coxb-property[@binding-type = 'CArrayPackedDecimalBinding']) > 0">
import com.legstar.coxb.rt.CArrayPackedDecimalBinding;</xsl:if>
<xsl:if test="count(coxb-property[@binding-type = 'CArrayZonedDecimalBinding']) > 0">
import com.legstar.coxb.rt.CArrayZonedDecimalBinding;</xsl:if>
<xsl:if test="count(coxb-property[@cobol-totalDigits]) > 0">
import java.math.BigDecimal;</xsl:if>
<xsl:if test="count(coxb-property[@jaxb-type = 'BigInteger' and @cobol-maxOccurs]) > 0">
import java.math.BigInteger;</xsl:if>
<xsl:if test="@type = 'complexArray'">
import com.legstar.coxb.rt.CArrayComplexBinding;
import java.util.List;</xsl:if>
<xsl:if test="@type = 'complexArray'">
import java.util.ArrayList;</xsl:if>

import <xsl:value-of select="jaxb-type-package"/>.<xsl:value-of select="jaxb-type-name"/>;
import <xsl:value-of select="jaxb-type-package"/>.ObjectFactory;

/**
 * This class was generated by COXB version 1.0.
 * <xsl:value-of  select="current-dateTime()"/>
 * This class implements a bi-directional binding between a cobol structure and
 * a java object. Visitors can use this class to visit each element of the
 * structure in turn. No reflection or annotations are used which makes this
 * class suitable for heavy loads.
 */
</xsl:template>

<!-- ===============================================================================================
   Generate the code of the java class that implements the binding
 -->
<xsl:template name="generate-class">
<xsl:param name="binding-class-name"/>
<xsl:variable name="extended-class">
  <xsl:choose>
     <xsl:when test="@type = 'choice'">CChoiceBinding</xsl:when>
     <xsl:when test="@type = 'complexArray'">CArrayComplexBinding</xsl:when>
    <xsl:otherwise>CComplexBinding</xsl:otherwise>
  </xsl:choose>
</xsl:variable>
<xsl:variable name="jaxbobject-class">
  <xsl:choose>
     <xsl:when test="@type = 'complexArray'"><xsl:value-of select="concat('List &lt; ',jaxb-type-name,' &gt;')"/></xsl:when>
    <xsl:otherwise><xsl:value-of select="jaxb-type-name"/></xsl:otherwise>
  </xsl:choose>
</xsl:variable>
public class <xsl:value-of select="$binding-class-name"/> 
             extends <xsl:value-of select="$extended-class"/>
             implements ICobolBinding {
  
    /** Name of java property to which this cobol element is bound. */
    private static final String JAVA_NAME = "<xsl:value-of select="jaxb-property-name"/>";
  
    /** Type of java property to which this cobol element is bound. */
    private static final String JAVA_TYPE = "<xsl:value-of select="jaxb-type-name"/>";
    <xsl:choose>
        <xsl:when test="@type = 'complexArray'">
    /** Minimum number of occurences for wrapped array. */
    private static final int MIN_OCCURS = <xsl:value-of select="coxb-property/@cobol-minOccurs"/>;
    
    /** Maximum number of occurences for wrapped array. */
    private static final int MAX_OCCURS = <xsl:value-of select="coxb-property/@cobol-maxOccurs"/>;
    
    <xsl:if test="coxb-property/@cobol-dependingOn">
    /** Variable giving the actual array size. */
    private static final String DEPENDING_ON = "<xsl:value-of select='coxb-property/@cobol-dependingOn'/>";
    </xsl:if>
    
    /** Binding for one of the complex array items. */
    private <xsl:value-of select="coxb-property/@binding-type"/><xsl:text> </xsl:text><xsl:value-of select="coxb-property/@jaxb-name"/>;
        </xsl:when>
        <xsl:otherwise>
            <xsl:for-each select="coxb-property">
    /** Child property <xsl:value-of select="@binding-type"/> of <xsl:value-of select="@type"/> type. */
    public <xsl:value-of select="@binding-type"/><xsl:text> </xsl:text><xsl:value-of select="@jaxb-name"/>;
            </xsl:for-each>
        </xsl:otherwise>
    </xsl:choose>
    /** Java object to which this cobol complex array element is bound. */
    private <xsl:value-of select="$jaxbobject-class"/> mJaxbObject;
  
    /** Java object factory for objects creation. */
    private ObjectFactory mObjectFactory;
  
    /**
     * No argument constructor.
     */
    public <xsl:value-of select="$binding-class-name"/>() {
        this(null, null, null);
    }
  
    /**
     * Constructor for a child complex binding which be later bound to a JAXB
     * object.
     * @param parentObject the parent binding
     */
    public <xsl:value-of select="$binding-class-name"/>(
        final CComplexBinding parentObject) {
        this(parentObject, null, null);
    }
  
    /**
     * Constructor for a root complex binding without an initial bound object.
     * @param objectFactory the java factory to use to create children instances
     */
    public <xsl:value-of select="$binding-class-name"/>(
        final ObjectFactory objectFactory) {
        this(null, objectFactory, null);
    }

    /**
     * Constructor for a root complex binding from an existing java object.
     * @param objectFactory the java factory to use to create children instances
     * @param jaxbObject the java object to which this element is bound
     */
    public <xsl:value-of select="$binding-class-name"/>(
        final ObjectFactory objectFactory,
        final <xsl:value-of select="$jaxbobject-class"/> jaxbObject) {
        this(null, objectFactory, jaxbObject);
    }

    /**
     * Constructor for a child complex binding with immediate binding to a JAXB
     * object.
     * @param parentObject the parent binding
     * @param objectFactory the java factory to use to create children instances
     * @param jaxbObject the java object to which this element is bound
     */
    public <xsl:value-of select="$binding-class-name"/>(
        final CComplexBinding parentObject,
        final ObjectFactory objectFactory,
        final <xsl:value-of select="$jaxbobject-class"/> jaxbObject) {
        
        super(JAVA_NAME, JAVA_TYPE, parentObject<xsl:if test="@type = 'complexArray'">, new <xsl:value-of select="coxb-property/@binding-type"/>(parentObject), MIN_OCCURS, MAX_OCCURS</xsl:if>);
        mJaxbObject = jaxbObject;
        mObjectFactory = objectFactory;
        initChildren();
    }
  
    /** Creates a binding property for each child. */
    public final void initChildren() {
    <xsl:choose>
        <xsl:when test="@type = 'complexArray'">
 <xsl:text>    </xsl:text><xsl:value-of select="coxb-property/@jaxb-name"/> = (<xsl:value-of select="coxb-property/@binding-type"/>) getComplexBinding();
        <xsl:if test="coxb-property/@cobol-dependingOn">
        setDependingOn(DEPENDING_ON);
        </xsl:if>
        </xsl:when>
        <xsl:when test="@type = 'complex'">
        /* Create binding children instances */
        <xsl:apply-templates select="coxb-property" mode="generate-property-init"/>
        /* Add children to children list */
           <xsl:for-each select="coxb-property">
        getChildrenList().add(<xsl:value-of select="@jaxb-name"/>);</xsl:for-each>
        </xsl:when>
        <xsl:when test="@type = 'choice'">
            <xsl:if test="string-length(@marshalChoiceStrategyClassName) &gt; 0">
        setMarshalChoiceStrategy(
            new <xsl:value-of select="@marshalChoiceStrategyClassName"/>());
        </xsl:if>
            <xsl:if test="string-length(@unmarshalChoiceStrategyClassName) &gt; 0">
        setUnmarshalChoiceStrategy(
            new <xsl:value-of select="@unmarshalChoiceStrategyClassName"/>());
        </xsl:if>
        /* Create binding alternatives instances */
        <xsl:apply-templates select="coxb-property" mode="generate-property-init"/>
        /* Add children to alternatives list */
            <xsl:for-each select="coxb-property">
        getAlternativesList().add(<xsl:value-of select="@jaxb-name"/>);</xsl:for-each>
        </xsl:when>
    </xsl:choose>
    }
  
   <xsl:choose>
        <xsl:when test="@type='complex'">
    /** {@inheritDoc} */
    public final void createBoundObject() throws HostException {
        setJaxbObject(mObjectFactory.create<xsl:value-of select="jaxb-type-name"/>());
    }
       </xsl:when>
        <xsl:when test="@type='choice'">
       </xsl:when>
        <xsl:when test="@type='complexArray'">
    /** {@inheritDoc} */
    public final void createBoundObject() throws HostException {
        setJaxbObject(new ArrayList &lt; <xsl:value-of select="jaxb-type-name"/> &gt;());
    }
    
    /** {@inheritDoc} */
    public final void initBoundItem(
        final int index) throws HostException {
    }
       </xsl:when>
    </xsl:choose>
    /** {@inheritDoc} */
    public final void prepareChildren() throws HostException {
    <xsl:for-each select="coxb-property[@type = 'complex']">
        <xsl:call-template name="create-complex-property"/>
    </xsl:for-each>
    <xsl:for-each select="coxb-property[@type = 'choice']">
        <xsl:call-template name="create-choice-property"/>
    </xsl:for-each>
    <xsl:for-each select="coxb-property[@type = 'complexArray']">
        <xsl:call-template name="create-complexarray-property"/>
    </xsl:for-each>
    }
  
   <xsl:choose>
        <xsl:when test="@type='complex' or @type='choice'">
    /** {@inheritDoc} */
    public final void getValuesFromBoundObject() throws HostException {
        /* Set this binding properties from java object property values */
         <xsl:for-each select="coxb-property">
            <xsl:call-template name="generate-property-marshal"/>
        </xsl:for-each>
    }
       </xsl:when>
        <xsl:when test="@type='complexArray'">
    /** {@inheritDoc} */
    public final void getValuesFromBoundItem(
        final int index) throws HostException {
        <xsl:value-of select="coxb-property/@jaxb-name"/>.setJaxbObject(mJaxbObject.get(index));
    }
       </xsl:when>
    </xsl:choose>

    <xsl:choose>
        <xsl:when test="@type='complex' or @type='choice'">
    /** {@inheritDoc} */
    public final void setBoundObjectValue(final int index) throws HostException {
        /* Set the JAXB object property value from binding object */
        switch (index) {<xsl:for-each select="coxb-property">
        case <xsl:value-of select="position() - 1"/>:
            setBoundObjectValue<xsl:value-of select="concat(upper-case(substring(@jaxb-name,1,1)),substring(@jaxb-name,2))"/>();
            break;</xsl:for-each>
        }
    }
            <xsl:apply-templates select="coxb-property" mode="generate-property-unmarshal"/>
       </xsl:when>
        <xsl:when test="@type='complexArray'">
    /** {@inheritDoc} */
    public final void setBoundItemValues(
        final int index) throws HostException {
        mJaxbObject.add(<xsl:value-of select="coxb-property/@jaxb-name"/>.getJaxbObject());
    }
       </xsl:when>
    </xsl:choose>
    /** {@inheritDoc} */
    public final Object getValue() throws HostException {
        return mJaxbObject;
    }

    /**
     * @return the java object to which this cobol complex element is bound
     */
    public final <xsl:value-of select="$jaxbobject-class"/> getJaxbObject() {
        return mJaxbObject;
    }

    /**
     * @param jaxbObject the java object to which this cobol complex element
     * is bound
     */
    public final void setJaxbObject(
        final <xsl:value-of select="$jaxbobject-class"/> jaxbObject) {
        mJaxbObject = jaxbObject;
    }

    /**
     * @return the java object factory for objects creation
     */
    public final ObjectFactory getObjectFactory() {
        return mObjectFactory;
    }

    /**
     * @param objectFactory the java object factory for objects creation to set
     */
    public final void setObjectFactory(final ObjectFactory objectFactory) {
        mObjectFactory = objectFactory;
    }

}
</xsl:template>

<!-- ===============================================================================================
   Generate the code to initialize individual properties
 -->
<xsl:template match="coxb-property" mode="generate-property-init">
  <xsl:choose>
    <xsl:when test="@binding-type = 'CStringBinding'">
        <xsl:value-of select="@jaxb-name"/> = new <xsl:value-of select="@binding-type"/>("<xsl:value-of select="@jaxb-name"/>", <xsl:value-of select="@cobol-byteLength"/>, <xsl:value-of select="@cobol-isJustifiedRight"/>);
        </xsl:when>
    <xsl:when test="@binding-type = 'CNationalBinding'">
        <xsl:value-of select="@jaxb-name"/> = new <xsl:value-of select="@binding-type"/>("<xsl:value-of select="@jaxb-name"/>", <xsl:value-of select="@cobol-byteLength"/>, <xsl:value-of select="@cobol-isJustifiedRight"/>);
        </xsl:when>
    <xsl:when test="@binding-type = 'CBinaryBinding'">
        <xsl:value-of select="@jaxb-name"/> = new <xsl:value-of select="@binding-type"/>("<xsl:value-of select="@jaxb-name"/>", <xsl:value-of select="@cobol-byteLength"/>, <xsl:value-of select="@cobol-totalDigits"/>, <xsl:value-of select="@cobol-fractionDigits"/>, <xsl:value-of select="@cobol-isSigned"/>);
        </xsl:when>
    <xsl:when test="@binding-type = 'CZonedDecimalBinding'">
        <xsl:value-of select="@jaxb-name"/> = new <xsl:value-of select="@binding-type"/>("<xsl:value-of select="@jaxb-name"/>", <xsl:value-of select="@cobol-byteLength"/>, <xsl:value-of select="@cobol-totalDigits"/>, <xsl:value-of select="@cobol-fractionDigits"/>, <xsl:value-of select="@cobol-isSigned"/>, <xsl:value-of select="@cobol-isSignLeading"/>, <xsl:value-of select="@cobol-isSignSeparate"/>);
        </xsl:when>
    <xsl:when test="@binding-type = 'CPackedDecimalBinding'">
        <xsl:value-of select="@jaxb-name"/> = new <xsl:value-of select="@binding-type"/>("<xsl:value-of select="@jaxb-name"/>", <xsl:value-of select="@cobol-byteLength"/>, <xsl:value-of select="@cobol-totalDigits"/>, <xsl:value-of select="@cobol-fractionDigits"/>, <xsl:value-of select="@cobol-isSigned"/>);
        </xsl:when>
    <xsl:when test="@binding-type = 'CDoubleBinding'">
        <xsl:value-of select="@jaxb-name"/> = new <xsl:value-of select="@binding-type"/>("<xsl:value-of select="@jaxb-name"/>");
        </xsl:when>
    <xsl:when test="@binding-type = 'CFloatBinding'">
        <xsl:value-of select="@jaxb-name"/> = new <xsl:value-of select="@binding-type"/>("<xsl:value-of select="@jaxb-name"/>");
        </xsl:when>
    <xsl:when test="@binding-type = 'COctetStreamBinding'">
        <xsl:value-of select="@jaxb-name"/> = new <xsl:value-of select="@binding-type"/>("<xsl:value-of select="@jaxb-name"/>", <xsl:value-of select="@cobol-byteLength"/>);
        </xsl:when>
    <xsl:when test="@binding-type = 'CArrayStringBinding'">
        <xsl:value-of select="@jaxb-name"/> = new <xsl:value-of select="@binding-type"/>("<xsl:value-of select="@jaxb-name"/>", this, <xsl:value-of select="@cobol-byteLength"/>, <xsl:value-of select="@cobol-isJustifiedRight"/>, <xsl:value-of select="@cobol-minOccurs"/>, <xsl:value-of select="@cobol-minOccurs"/>);
        </xsl:when>
    <xsl:when test="@binding-type = 'CArrayNationalBinding'">
        <xsl:value-of select="@jaxb-name"/> = new <xsl:value-of select="@binding-type"/>("<xsl:value-of select="@jaxb-name"/>", this, <xsl:value-of select="@cobol-byteLength"/>, <xsl:value-of select="@cobol-isJustifiedRight"/>, <xsl:value-of select="@cobol-minOccurs"/>, <xsl:value-of select="@cobol-minOccurs"/>);
        </xsl:when>
    <xsl:when test="@binding-type = 'CArrayBinaryBinding'">
        <xsl:value-of select="@jaxb-name"/> = new <xsl:value-of select="@binding-type"/>("<xsl:value-of select="@jaxb-name"/>", this, <xsl:value-of select="@cobol-byteLength"/>, <xsl:value-of select="@cobol-totalDigits"/>, <xsl:value-of select="@cobol-fractionDigits"/>, <xsl:value-of select="@cobol-isSigned"/>, <xsl:value-of select="@cobol-minOccurs"/>, <xsl:value-of select="@cobol-minOccurs"/>);
        </xsl:when>
    <xsl:when test="@binding-type = 'CArrayZonedDecimalBinding'">
        <xsl:value-of select="@jaxb-name"/> = new <xsl:value-of select="@binding-type"/>("<xsl:value-of select="@jaxb-name"/>", this, <xsl:value-of select="@cobol-byteLength"/>, <xsl:value-of select="@cobol-totalDigits"/>, <xsl:value-of select="@cobol-fractionDigits"/>, <xsl:value-of select="@cobol-isSigned"/>, <xsl:value-of select="@cobol-isSignLeading"/>, <xsl:value-of select="@cobol-isSignSeparate"/>, <xsl:value-of select="@cobol-minOccurs"/>, <xsl:value-of select="@cobol-minOccurs"/>);
        </xsl:when>
    <xsl:when test="@binding-type = 'CArrayPackedDecimalBinding'">
        <xsl:value-of select="@jaxb-name"/> = new <xsl:value-of select="@binding-type"/>("<xsl:value-of select="@jaxb-name"/>", this, <xsl:value-of select="@cobol-byteLength"/>, <xsl:value-of select="@cobol-totalDigits"/>, <xsl:value-of select="@cobol-fractionDigits"/>, <xsl:value-of select="@cobol-isSigned"/>, <xsl:value-of select="@cobol-minOccurs"/>, <xsl:value-of select="@cobol-minOccurs"/>);
        </xsl:when>
    <xsl:when test="@binding-type = 'CArrayDoubleBinding'">
        <xsl:value-of select="@jaxb-name"/> = new <xsl:value-of select="@binding-type"/>("<xsl:value-of select="@jaxb-name"/>", this, <xsl:value-of select="@cobol-minOccurs"/>, <xsl:value-of select="@cobol-minOccurs"/>);
        </xsl:when>
    <xsl:when test="@binding-type = 'CArrayFloatBinding'">
        <xsl:value-of select="@jaxb-name"/> = new <xsl:value-of select="@binding-type"/>("<xsl:value-of select="@jaxb-name"/>", this, <xsl:value-of select="@cobol-minOccurs"/>, <xsl:value-of select="@cobol-minOccurs"/>);
        </xsl:when>
    <xsl:when test="@binding-type = 'CArrayOctetStreamBinding'">
        <xsl:value-of select="@jaxb-name"/> = new <xsl:value-of select="@binding-type"/>("<xsl:value-of select="@jaxb-name"/>", this, <xsl:value-of select="@cobol-byteLength"/>, <xsl:value-of select="@cobol-minOccurs"/>, <xsl:value-of select="@cobol-minOccurs"/>);
        </xsl:when>
    <xsl:otherwise>
      <xsl:choose>
        <xsl:when test="../@type = 'choice'">
            <xsl:value-of select="@jaxb-name"/> = new <xsl:value-of select="@binding-type"/>(getParentBinding());
        </xsl:when>
        <xsl:otherwise>
            <xsl:value-of select="@jaxb-name"/> = new <xsl:value-of select="@binding-type"/>(this);
        </xsl:otherwise>
      </xsl:choose>
    </xsl:otherwise>
  </xsl:choose>

  <xsl:if test="@cobol-name">
    <xsl:value-of select="@jaxb-name"/>.setCobolName("<xsl:value-of select="@cobol-name"/>");
        </xsl:if>
  <xsl:if test="@cobol-isRedefined = 'true'">
    <xsl:value-of select="@jaxb-name"/>.setIsRedefined(true);
        </xsl:if>
  <xsl:if test="@cobol-redefines">
    <xsl:value-of select="@jaxb-name"/>.setRedefines("<xsl:value-of select="@cobol-redefines"/>");
        </xsl:if>
  <xsl:if test="@cobol-isODOObject = 'true'">
    <xsl:value-of select="@jaxb-name"/>.setIsODOObject(true);
        </xsl:if>
  <xsl:if test="@cobol-dependingOn">
    <xsl:value-of select="@jaxb-name"/>.setDependingOn("<xsl:value-of select="@cobol-dependingOn"/>");
        </xsl:if>
  <xsl:if test="@cobol-isCustomVariable">
    <xsl:value-of select="@jaxb-name"/>.setIsCustomVariable(<xsl:value-of select="@cobol-isCustomVariable"/>);
        </xsl:if>
  
</xsl:template>

<!-- ===============================================================================================
   Generate the code to set binding values from JAXB instance properties
 -->
<xsl:template name="generate-property-marshal">
    <xsl:variable name="jaxb-getter-method">
      <xsl:value-of select="concat('get',upper-case(substring(@jaxb-name,1,1)),substring(@jaxb-name,2))"/>
    </xsl:variable>
  <xsl:variable name="jaxb-create-method">
    <xsl:value-of select="concat('create',@jaxb-type)"/>
  </xsl:variable>
    <xsl:variable name="uninitialized-value">
      <xsl:choose>
         <xsl:when test="@jaxb-type = 'byte' or @jaxb-type = 'short' or @jaxb-type = 'int'">0</xsl:when>
         <xsl:when test="@jaxb-type = 'long'">0L</xsl:when>
         <xsl:when test="@jaxb-type = 'float'">0f</xsl:when>
         <xsl:when test="@jaxb-type = 'double'">0d</xsl:when>
        <xsl:otherwise>null</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
<xsl:text>            </xsl:text>
  <xsl:choose>
    <xsl:when test="@binding-type = 'CStringBinding' or @binding-type = 'COctetStreamBinding' or @binding-type = 'CNationalBinding'">
        if (mJaxbObject.<xsl:value-of select="$jaxb-getter-method"/>() != <xsl:value-of select="$uninitialized-value"/>) {
            /* Set value from <xsl:value-of select="@jaxb-name"/>*/
<xsl:text>            </xsl:text><xsl:value-of select="@jaxb-name"/>.setValue(mJaxbObject.<xsl:value-of select="$jaxb-getter-method"/>());
        }
    </xsl:when>
    <xsl:when test="@binding-type = 'CDoubleBinding' or @binding-type = 'CFloatBinding'">
        if (mJaxbObject.<xsl:value-of select="$jaxb-getter-method"/>() != <xsl:value-of select="$uninitialized-value"/>) {
            /* Set value from <xsl:value-of select="@jaxb-name"/>*/
<xsl:text>            </xsl:text><xsl:value-of select="@jaxb-name"/>.setValue(mJaxbObject.<xsl:value-of select="$jaxb-getter-method"/>());
        }
    </xsl:when>
    <xsl:when test="@binding-type = 'CBinaryBinding' or @binding-type = 'CZonedDecimalBinding' or @binding-type = 'CPackedDecimalBinding'">
        if (mJaxbObject.<xsl:value-of select="$jaxb-getter-method"/>() != <xsl:value-of select="$uninitialized-value"/>) {
            /* Set value from <xsl:value-of select="@jaxb-name"/>*/
<xsl:text>            </xsl:text>
      <xsl:choose>
        <xsl:when test="@jaxb-type = 'BigDecimal'">
          <xsl:value-of select="@jaxb-name"/>.setValue(mJaxbObject.<xsl:value-of select="$jaxb-getter-method"/>());
        </xsl:when>
        <xsl:when test="@jaxb-type = 'BigInteger'">
          <xsl:value-of select="@jaxb-name"/>.setValue(new BigDecimal(mJaxbObject.<xsl:value-of select="$jaxb-getter-method"/>()));
        </xsl:when>
        <xsl:when test="@jaxb-type = 'byte' or @jaxb-type = 'short' or @jaxb-type = 'int' or @jaxb-type = 'long'">
          <xsl:value-of select="@jaxb-name"/>.setValue(new BigDecimal(mJaxbObject.<xsl:value-of select="$jaxb-getter-method"/>()));
        </xsl:when>
        <xsl:when test="@jaxb-type = 'Byte' or @jaxb-type = 'Short' or @jaxb-type = 'Integer' or @jaxb-type = 'Long'">
          <xsl:value-of select="@jaxb-name"/>.setValue(new BigDecimal(mJaxbObject.<xsl:value-of select="$jaxb-getter-method"/>()));
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="@jaxb-name"/>.setValue(mJaxbObject.<xsl:value-of select="$jaxb-getter-method"/>());
        </xsl:otherwise>
      </xsl:choose>
        }
    </xsl:when>
    <xsl:when test="@binding-type = 'CArrayStringBinding' or @binding-type = 'CArrayOctetStreamBinding' or @binding-type = 'CArrayNationalBinding'">
        if (mJaxbObject.<xsl:value-of select="$jaxb-getter-method"/>() != <xsl:value-of select="$uninitialized-value"/>) {
            /* Set value from <xsl:value-of select="@jaxb-name"/>*/
<xsl:text>            </xsl:text><xsl:value-of select="@jaxb-name"/>.setValue(mJaxbObject.<xsl:value-of select="$jaxb-getter-method"/>());
        }
    </xsl:when>
    <xsl:when test="@binding-type = 'CArrayDoubleBinding' or @binding-type = 'CArrayFloatBinding'">
        if (mJaxbObject.<xsl:value-of select="$jaxb-getter-method"/>() != <xsl:value-of select="$uninitialized-value"/>) {
            /* Set value from <xsl:value-of select="@jaxb-name"/>*/
<xsl:text>            </xsl:text><xsl:value-of select="@jaxb-name"/>.setValue(mJaxbObject.<xsl:value-of select="$jaxb-getter-method"/>());
        }
    </xsl:when>
    <xsl:when test="@binding-type = 'CArrayBinaryBinding' or @binding-type = 'CArrayZonedDecimalBinding' or @binding-type = 'CArrayPackedDecimalBinding'">
        if (mJaxbObject.<xsl:value-of select="$jaxb-getter-method"/>() != <xsl:value-of select="$uninitialized-value"/>) {
            /* Set value from <xsl:value-of select="@jaxb-name"/>*/
      <xsl:choose>
        <xsl:when test="@jaxb-type = 'BigDecimal'">
 <xsl:text>        </xsl:text><xsl:value-of select="@jaxb-name"/>.setValue(mJaxbObject.<xsl:value-of select="$jaxb-getter-method"/>());
        </xsl:when>
        <xsl:otherwise>
            if (<xsl:value-of select="@jaxb-name"/>.getList() != null) {
<xsl:text>                </xsl:text><xsl:value-of select="@jaxb-name"/>.getList().clear();
            } else {
<xsl:text>                </xsl:text><xsl:value-of select="@jaxb-name"/>.setList(new java.util.ArrayList &lt; BigDecimal &gt;());
            }
            for (<xsl:value-of select="@jaxb-type"/> item : mJaxbObject.<xsl:value-of select="$jaxb-getter-method"/>()) {
<xsl:text>                </xsl:text><xsl:value-of select="@jaxb-name"/>.getList().add(new BigDecimal(item));
            }
        </xsl:otherwise>
      </xsl:choose>
        }
    </xsl:when>
    <xsl:otherwise>
      <xsl:choose>
        <xsl:when test="@type = 'complex'">
        if (mJaxbObject.<xsl:value-of select="$jaxb-getter-method"/>() != <xsl:value-of select="$uninitialized-value"/>) {
            /* Set value from <xsl:value-of select="@jaxb-name"/>*/
<xsl:text>            </xsl:text><xsl:value-of select="@jaxb-name"/>.setJaxbObject(mJaxbObject.<xsl:value-of select="$jaxb-getter-method"/>());
            <xsl:choose>
                <xsl:when test="@cobol-isRedefined = 'true' or string-length(@cobol-redefines) > 0">
        }
                </xsl:when>
                <xsl:otherwise>
        } else {
<xsl:text>            </xsl:text><xsl:value-of select="@jaxb-name"/>.setJaxbObject(mObjectFactory.<xsl:value-of select="$jaxb-create-method"/>());
        }
                </xsl:otherwise>
            </xsl:choose>
        </xsl:when>
        <xsl:when test="@type = 'complexArray'">
        if (mJaxbObject.<xsl:value-of select="$jaxb-getter-method"/>() != <xsl:value-of select="$uninitialized-value"/>) {
            /* Set value from <xsl:value-of select="@jaxb-name"/>*/
<xsl:text>            </xsl:text><xsl:value-of select="@jaxb-name"/>.setJaxbObject(mJaxbObject.<xsl:value-of select="$jaxb-getter-method"/>());
            <xsl:choose>
                <xsl:when test="@cobol-isRedefined = 'true' or string-length(@cobol-redefines) > 0">
        }
                </xsl:when>
                <xsl:otherwise>
        } else {
<xsl:text>            </xsl:text><xsl:value-of select="@jaxb-name"/>.createBoundObject();
        }
                </xsl:otherwise>
            </xsl:choose>
        </xsl:when>
      </xsl:choose>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<!-- ===============================================================================================
   Generate the code to set JAXB instance properties from binding values 
 -->
<xsl:template match="coxb-property" mode="generate-property-unmarshal">
    <xsl:variable name="jaxb-setter-method">
        <xsl:value-of select="concat('set',upper-case(substring(@jaxb-name,1,1)),substring(@jaxb-name,2))"/>
    </xsl:variable>
    <xsl:variable name="jaxb-getter-method">
        <xsl:value-of select="concat('get',upper-case(substring(@jaxb-name,1,1)),substring(@jaxb-name,2))"/>
    </xsl:variable>
    /** Set corresponding JAXB object property value. */
    private final void setBoundObjectValue<xsl:value-of select="concat(upper-case(substring(@jaxb-name,1,1)),substring(@jaxb-name,2))"/>() throws HostException {
    <xsl:choose>
        <xsl:when test="@binding-type = 'CStringBinding' or @binding-type = 'COctetStreamBinding' or @binding-type = 'CNationalBinding'">
        if (<xsl:value-of select="@jaxb-name"/>.getValue() != null) {
            /* Set value of <xsl:value-of select="@jaxb-name"/>*/
            mJaxbObject.<xsl:value-of select="$jaxb-setter-method"/>(<xsl:value-of select="@jaxb-name"/>.getValue());
        }
        </xsl:when>
        <xsl:when test="@binding-type = 'CDoubleBinding' or @binding-type = 'CFloatBinding'">
        if (<xsl:value-of select="@jaxb-name"/>.getValue() != null) {
            /* Set value of <xsl:value-of select="@jaxb-name"/>*/
            mJaxbObject.<xsl:value-of select="$jaxb-setter-method"/>(<xsl:value-of select="@jaxb-name"/>.getValue());
        }
        </xsl:when>
        <xsl:when test="@binding-type = 'CBinaryBinding' or @binding-type = 'CZonedDecimalBinding' or @binding-type = 'CPackedDecimalBinding'">
        if (<xsl:value-of select="@jaxb-name"/>.getValue() != null) {
            <xsl:choose>
                <xsl:when test="@jaxb-type = 'BigDecimal'">
            /* Set value of <xsl:value-of select="@jaxb-name"/>*/
            mJaxbObject.<xsl:value-of select="$jaxb-setter-method"/>(<xsl:value-of select="@jaxb-name"/>.getValue());
                </xsl:when>
                <xsl:when test="@jaxb-type = 'BigInteger'">
            /* Set value of <xsl:value-of select="@jaxb-name"/>*/
            mJaxbObject.<xsl:value-of select="$jaxb-setter-method"/>(<xsl:value-of select="@jaxb-name"/>.getValue().toBigInteger());
                </xsl:when>
                <xsl:when test="@jaxb-type = 'byte' or @jaxb-type = 'short' or @jaxb-type = 'int' or @jaxb-type = 'long'">
            /* Set value of <xsl:value-of select="@jaxb-name"/>*/
            mJaxbObject.<xsl:value-of select="$jaxb-setter-method"/>(<xsl:value-of select="@jaxb-name"/>.getValue().<xsl:value-of select="@jaxb-type"/>ValueExact());
                </xsl:when>
                <xsl:when test="@jaxb-type = 'Byte' or @jaxb-type = 'Short' or @jaxb-type = 'Long'">
            /* Set value of <xsl:value-of select="@jaxb-name"/>*/
            mJaxbObject.<xsl:value-of select="$jaxb-setter-method"/>(<xsl:value-of select="@jaxb-name"/>.getValue().<xsl:value-of select="concat(lower-case(substring(@jaxb-type,1,1)),substring(@jaxb-type,2))"/>ValueExact());
                </xsl:when>
                <xsl:when test="@jaxb-type = 'Integer'">
            /* Set value of <xsl:value-of select="@jaxb-name"/>*/
            mJaxbObject.<xsl:value-of select="$jaxb-setter-method"/>(<xsl:value-of select="@jaxb-name"/>.getValue().intValueExact());
                </xsl:when>
                <xsl:otherwise>
            /* Set value of <xsl:value-of select="@jaxb-name"/>*/
            mJaxbObject.<xsl:value-of select="$jaxb-setter-method"/>(<xsl:value-of select="@jaxb-name"/>.getJaxbObject());
                </xsl:otherwise>
            </xsl:choose>
        }
        </xsl:when>
        <xsl:when test="@binding-type = 'CArrayStringBinding' or @binding-type = 'CArrayOctetStreamBinding' or @binding-type = 'CArrayNationalBinding'">
        if (<xsl:value-of select="@jaxb-name"/>.getValue() != null) {
            /* Set value of <xsl:value-of select="@jaxb-name"/>*/
            mJaxbObject.<xsl:value-of select="$jaxb-getter-method"/>().clear();
            mJaxbObject.<xsl:value-of select="$jaxb-getter-method"/>().addAll(<xsl:value-of select="@jaxb-name"/>.getValue());
        }
        </xsl:when>
        <xsl:when test="@binding-type = 'CArrayDoubleBinding' or @binding-type = 'CArrayFloatBinding'">
        if (<xsl:value-of select="@jaxb-name"/>.getValue() != null) {
            /* Set value of <xsl:value-of select="@jaxb-name"/>*/
            mJaxbObject.<xsl:value-of select="$jaxb-getter-method"/>().clear();
            mJaxbObject.<xsl:value-of select="$jaxb-getter-method"/>().addAll(<xsl:value-of select="@jaxb-name"/>.getValue());
        }
        </xsl:when>
        <xsl:when test="@binding-type = 'CArrayBinaryBinding' or @binding-type = 'CArrayZonedDecimalBinding' or @binding-type = 'CArrayPackedDecimalBinding'">
        if (<xsl:value-of select="@jaxb-name"/>.getValue() != null) {
            <xsl:choose>
                <xsl:when test="@jaxb-type = 'BigDecimal'">
            /* Set value of <xsl:value-of select="@jaxb-name"/>*/
            mJaxbObject.<xsl:value-of select="$jaxb-getter-method"/>().clear();
            mJaxbObject.<xsl:value-of select="$jaxb-getter-method"/>().addAll(<xsl:value-of select="@jaxb-name"/>.getValue());
                </xsl:when>
                <xsl:when test="@jaxb-type = 'BigInteger'">
            /* Set value of <xsl:value-of select="@jaxb-name"/>*/
            mJaxbObject.<xsl:value-of select="$jaxb-getter-method"/>().clear();
            for (BigDecimal item : <xsl:value-of select="@jaxb-name"/>.getValue()) {
               mJaxbObject.<xsl:value-of select="$jaxb-getter-method"/>().add(item.toBigInteger());
            }
                </xsl:when>
                <xsl:when test="@jaxb-type = 'Integer'">
            /* Set value of <xsl:value-of select="@jaxb-name"/>*/
            mJaxbObject.<xsl:value-of select="$jaxb-getter-method"/>().clear();
            for (BigDecimal item : <xsl:value-of select="@jaxb-name"/>.getValue()) {
                mJaxbObject.<xsl:value-of select="$jaxb-getter-method"/>().add(new <xsl:value-of select="@jaxb-type"/>(item.intValueExact()));
            }
                </xsl:when>
                <xsl:otherwise>
            /* Set value of <xsl:value-of select="@jaxb-name"/>*/
            mJaxbObject.<xsl:value-of select="$jaxb-getter-method"/>().clear();
            for (BigDecimal item : <xsl:value-of select="@jaxb-name"/>.getValue()) {
                mJaxbObject.<xsl:value-of select="$jaxb-getter-method"/>().add(new <xsl:value-of select="concat(upper-case(substring(@jaxb-type,1,1)),substring(@jaxb-type,2))"/>(item.<xsl:value-of select="concat(lower-case(substring(@jaxb-type,1,1)),substring(@jaxb-type,2))"/>ValueExact()));
            }
                </xsl:otherwise>
            </xsl:choose>
        }
        </xsl:when>
        <xsl:when test="@type = 'complex'">
        /* Set value of complex child <xsl:value-of select="@jaxb-name"/>*/
        mJaxbObject.<xsl:value-of select="$jaxb-setter-method"/>(<xsl:value-of select="@jaxb-name"/>.getJaxbObject());
        </xsl:when>
        <xsl:when test="@type = 'complexArray'">
        /* Set value of complex array child <xsl:value-of select="@jaxb-name"/>*/
        mJaxbObject.<xsl:value-of select="$jaxb-getter-method"/>().clear();
        mJaxbObject.<xsl:value-of select="$jaxb-getter-method"/>().addAll(<xsl:value-of select="@jaxb-name"/>.getJaxbObject());
        </xsl:when>
    </xsl:choose>
    }
</xsl:template>

<!-- ===============================================================================================
   Generate the code to set JAXB instance for a complex element child 
 -->
<xsl:template name="create-complex-property">
  <xsl:variable name="jaxb-create-method">
    <xsl:value-of select="concat('create',@jaxb-type)"/>
  </xsl:variable>
        /* Pass on the JAXB factory to child <xsl:value-of select="@jaxb-name"/>  */
<xsl:text>        </xsl:text><xsl:value-of select="@jaxb-name"/>.setObjectFactory(mObjectFactory);
</xsl:template>

<!-- ===============================================================================================
   Generate the code to set JAXB instance for a choice element child
 -->
<xsl:template name="create-choice-property">
        /* Child <xsl:value-of select="@jaxb-name"/> is a choice. Because JAXB does not create
         * objects for choices, we propagate the parent object. */
<xsl:text>        </xsl:text><xsl:value-of select="@jaxb-name"/>.setJaxbObject(mJaxbObject);
<xsl:text>        </xsl:text><xsl:value-of select="@jaxb-name"/>.setObjectFactory(mObjectFactory);
</xsl:template>

<!-- ===============================================================================================
   Generate the code to set JAXB instance for a complex array element child
 -->
<xsl:template name="create-complexarray-property">
        /* Pass on the JAXB factory to child <xsl:value-of select="@jaxb-name"/>  */
<xsl:text>        </xsl:text><xsl:value-of select="@jaxb-name"/>.setObjectFactory(mObjectFactory);
</xsl:template>

<!-- ===============================================================================================
   Generate a choice stragegy skeletton source 
 -->
<xsl:template match="coxb-type" mode="choiceStrategy">
<xsl:param name="choice-strategy-class-qual-name"/>
<xsl:param name="choice-strategy-class-exists"/>

  <!-- Determine the choice strategy java source file name and location -->
  <xsl:variable name="path" select="tokenize($choice-strategy-class-qual-name,'\.')"/>
  
  <!-- If the strategy class name is fully qualified, separate package name from class name
       otherwise, use the same binding package as caller.  -->
  <xsl:variable name="choice-strategy-class-name">
    <xsl:value-of select="$path[count($path)]"/>
  </xsl:variable>
  <xsl:variable name="choice-strategy-type-package">
    <xsl:choose>
      <xsl:when test="count($path) > 1">
        <xsl:value-of select="substring($choice-strategy-class-qual-name,1,(string-length($choice-strategy-class-qual-name) - string-length($choice-strategy-class-name) - 1))"/>
      </xsl:when>
      <xsl:otherwise>
            <xsl:value-of select="concat(jaxb-type-package,'.bind')"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <xsl:variable name="choice-strategy-target-dir">
    <xsl:value-of select="translate($choice-strategy-type-package,'.','/')"/>
  </xsl:variable>
  <!-- To protect custom code, the generated skeleton takes a non java
       extension.  -->
  <xsl:variable name="choice-strategy-extension">
    <xsl:choose>
      <xsl:when test="$choice-strategy-class-exists = 'true'">java.new</xsl:when>
      <xsl:otherwise>java</xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  
  
  
  <!-- Generate the dynamically built java source file -->
  <xsl:result-document href="{$choice-strategy-target-dir}/{$choice-strategy-class-name}.{$choice-strategy-extension}" method="text" omit-xml-declaration="yes" indent="yes">
package <xsl:value-of select="$choice-strategy-type-package"/>;
import java.util.Hashtable;

import com.legstar.coxb.CobolElementVisitor;
import com.legstar.coxb.ICobolBinding;
import com.legstar.coxb.ICobolChoiceBinding;
import com.legstar.coxb.ICobolUnmarshalChoiceStrategy;
import com.legstar.host.HostException;
import <xsl:value-of select="concat(jaxb-type-package, '.', jaxb-type-name)"/>;

/** 
 * Skeleton implementation of a custom choice selection strategy. Modify this
 * code to select a suitable alternative.
 */
public class <xsl:value-of select="$choice-strategy-class-name"/> implements ICobolUnmarshalChoiceStrategy {

   /* (non-Javadoc)
    * @see com.legstar.coxb.ICobolUnmarshalChoiceStrategy#choose(com.legstar.coxb.ICobolChoiceBinding, java.util.Hashtable, com.legstar.coxb.CobolElementVisitor)
    */
    public final ICobolBinding choose(
        final ICobolChoiceBinding choice,
        final Hashtable &lt; String, Object &gt; variablesMap,
        CobolElementVisitor visitor) throws HostException {
    
        /* Get the parent JAXB object which properties might help select the
         * right alternative. */
        <xsl:value-of select="jaxb-type-name"/> jaxbo = (<xsl:value-of select="jaxb-type-name"/>) choice.getValue();
    
        /* Replace following code with actual logic. */
        int index = 0;
        switch (index) {<xsl:for-each select="coxb-property">
        case <xsl:value-of select="position() - 1"/>:
            return choice.getAlternativeByJavaName("<xsl:value-of select="@jaxb-name"/>");</xsl:for-each>
        case -1:
            /* An exemple of how to signal an exception.*/
            throw (new HostException("Unable to select an alternative"));
        default:
            /* Null will let the default choice strategy apply. */
            return null;
    }
  }

}
  </xsl:result-document>

</xsl:template>

</xsl:stylesheet>

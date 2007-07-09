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
    <xsl:value-of select="concat(@jaxb-package,'.bind')"/>
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

import com.legstar.coxb.host.HostException;
import com.legstar.coxb.ICobolComplexBinding;
<xsl:if test="@type='complex' or @type='choice'">
import com.legstar.coxb.ICobolBinding;
<xsl:if test="count(coxb-property[@type = 'simple']) > 0">
import com.legstar.coxb.CobolBindingFactory;
import com.legstar.coxb.ICobolBindingFactory;</xsl:if></xsl:if>
<xsl:if test="@type='choice'">
import com.legstar.coxb.ICobolChoiceBinding;</xsl:if>
<xsl:if test="@type='complex'">
import com.legstar.coxb.common.CComplexBinding;</xsl:if>
<xsl:if test="@type='choice'">
import com.legstar.coxb.common.CChoiceBinding;</xsl:if>
<xsl:if test="count(coxb-property[@binding-type = 'ICobolBinaryBinding']) > 0">
import com.legstar.coxb.ICobolBinaryBinding;</xsl:if>
<xsl:if test="count(coxb-property[@binding-type = 'ICobolDoubleBinding']) > 0">
import com.legstar.coxb.ICobolDoubleBinding;</xsl:if>
<xsl:if test="count(coxb-property[@binding-type = 'ICobolFloatBinding']) > 0">
import com.legstar.coxb.ICobolFloatBinding;</xsl:if>
<xsl:if test="count(coxb-property[@binding-type = 'ICobolOctetStreamBinding']) > 0">
import com.legstar.coxb.ICobolOctetStreamBinding;</xsl:if>
<xsl:if test="count(coxb-property[@binding-type = 'ICobolStringBinding']) > 0">
import com.legstar.coxb.ICobolStringBinding;</xsl:if>
<xsl:if test="count(coxb-property[@binding-type = 'ICobolNationalBinding']) > 0">
import com.legstar.coxb.ICobolNationalBinding;</xsl:if>
<xsl:if test="count(coxb-property[@binding-type = 'ICobolPackedDecimalBinding']) > 0">
import com.legstar.coxb.ICobolPackedDecimalBinding;</xsl:if>
<xsl:if test="count(coxb-property[@binding-type = 'ICobolZonedDecimalBinding']) > 0">
import com.legstar.coxb.ICobolZonedDecimalBinding;</xsl:if>
<xsl:if test="count(coxb-property[@binding-type = 'ICobolArrayBinaryBinding']) > 0">
import com.legstar.coxb.ICobolArrayBinaryBinding;</xsl:if>
<xsl:if test="count(coxb-property[@binding-type = 'ICobolArrayDoubleBinding']) > 0">
import com.legstar.coxb.ICobolArrayDoubleBinding;</xsl:if>
<xsl:if test="count(coxb-property[@binding-type = 'ICobolArrayFloatBinding']) > 0">
import com.legstar.coxb.ICobolArrayFloatBinding;</xsl:if>
<xsl:if test="count(coxb-property[@binding-type = 'ICobolArrayOctetStreamBinding']) > 0">
import com.legstar.coxb.ICobolArrayOctetStreamBinding;</xsl:if>
<xsl:if test="count(coxb-property[@binding-type = 'ICobolArrayStringBinding']) > 0">
import com.legstar.coxb.ICobolArrayStringBinding;</xsl:if>
<xsl:if test="count(coxb-property[@binding-type = 'ICobolArrayNationalBinding']) > 0">
import com.legstar.coxb.ICobolArrayNationalBinding;</xsl:if>
<xsl:if test="count(coxb-property[@binding-type = 'ICobolArrayPackedDecimalBinding']) > 0">
import com.legstar.coxb.ICobolArrayPackedDecimalBinding;</xsl:if>
<xsl:if test="count(coxb-property[@binding-type = 'ICobolArrayZonedDecimalBinding']) > 0">
import com.legstar.coxb.ICobolArrayZonedDecimalBinding;</xsl:if>
<xsl:if test="count(coxb-property[@jaxb-type = 'BigDecimal']) > 0">
import java.math.BigDecimal;</xsl:if>
<xsl:if test="count(coxb-property[@jaxb-type = 'BigInteger']) > 0">
import java.math.BigInteger;</xsl:if>
<xsl:if test="@type = 'complexArray'">
import com.legstar.coxb.common.CArrayComplexBinding;</xsl:if>
<xsl:if test="@type = 'complexArray' or count(coxb-property[@type = 'complexArray' or @cobol-maxOccurs > 0]) > 0">
import java.util.List;</xsl:if>
<xsl:if test="@type = 'complexArray'">
import java.util.ArrayList;</xsl:if>
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
<xsl:choose>
    <xsl:when test="@type = 'complex'">
import <xsl:value-of select="@jaxb-package"/>.ObjectFactory;
import <xsl:value-of select="@jaxb-package"/>.<xsl:value-of select="@jaxb-type"/>;
<xsl:for-each select="coxb-property[@type = 'complex' or @type = 'complexArray']">
import <xsl:value-of select="../@jaxb-package"/>.<xsl:value-of select="@jaxb-type"/>;</xsl:for-each>
    </xsl:when>
    <xsl:when test="@type = 'choice'">
import <xsl:value-of select="@jaxb-package"/>.<xsl:value-of select="@parent-jaxb-type"/>;
<xsl:for-each select="coxb-property[@type = 'complex' or @type = 'complexArray']">
import <xsl:value-of select="../@jaxb-package"/>.<xsl:value-of select="@jaxb-type"/>;</xsl:for-each>
    </xsl:when>
    <xsl:when test="@type = 'complexArray'">
import <xsl:value-of select="@jaxb-package"/>.<xsl:value-of select="@item-jaxb-type"/>;
    </xsl:when>
</xsl:choose>

/**
 * This class was generated by LegStar coxbgen version 1.1.
 * <xsl:value-of  select="current-dateTime()"/>
</xsl:template>

<!-- ===============================================================================================
   Determine which template applies based on the binding type
 -->
<xsl:template name="generate-class">
<xsl:param name="binding-class-name"/>
    <xsl:choose>
        <xsl:when test="@type = 'choice'">
            <xsl:call-template name="generate-choice-class">
                <xsl:with-param name="binding-class-name"><xsl:value-of select="$binding-class-name"/></xsl:with-param>
            </xsl:call-template>
        </xsl:when>
        <xsl:when test="@type = 'complexArray'">
            <xsl:call-template name="generate-complex-array-class">
                <xsl:with-param name="binding-class-name"><xsl:value-of select="$binding-class-name"/></xsl:with-param>
            </xsl:call-template>
        </xsl:when>
        <xsl:otherwise>
            <xsl:call-template name="generate-complex-class">
                <xsl:with-param name="binding-class-name"><xsl:value-of select="$binding-class-name"/></xsl:with-param>
            </xsl:call-template>
        </xsl:otherwise>
    </xsl:choose>
</xsl:template>

<!-- ===============================================================================================
   Generate the code of the java class that implements a complex binding
 -->
<xsl:template name="generate-complex-class">
<xsl:param name="binding-class-name"/>
 * This class implements a bi-directional binding between a cobol structure and
 * a java object.
 */

public class <xsl:value-of select="$binding-class-name"/> 
             extends CComplexBinding {

    /** Jaxb object to which this cobol complex element is bound. */
    private <xsl:value-of select="@jaxb-type"/> mJaxbObject;
  
    /** Indicates that the associated Jaxb object just came from the constructor
     * and doesn't need to be recreated. */
    private boolean mUnusedJaxbObject = false;
    
    /** Children of this complex binding. */
    <xsl:for-each select="coxb-property">
    /** Child bound to jaxb property <xsl:value-of select="@jaxb-name"/>(<xsl:value-of select="@jaxb-type"/>). */
    public <xsl:value-of select="@binding-type"/><xsl:text> </xsl:text><xsl:value-of select="@var-name"/>;</xsl:for-each>
    <xsl:for-each select="coxb-property[@type = 'complexArray']">
    /** Binding item for complex array binding <xsl:value-of select="@binding-type"/>. */
    public <xsl:value-of select="@item-binding-type"/><xsl:text> </xsl:text><xsl:value-of select="@var-name"/>Item;</xsl:for-each>
            
    /** Logger. */
    private static final Log LOG
        = LogFactory.getLog(<xsl:value-of select="$binding-class-name"/>.class);
    <xsl:if test="count(coxb-property[@type = 'simple']) > 0">
    /** Binding factory. */
    private static final ICobolBindingFactory BF
        = CobolBindingFactory.getBindingFactory();</xsl:if>
    
    /** Static reference to Jaxb object factory to be used as default. */
    private static final ObjectFactory JF = new ObjectFactory();
    
    /** Current Jaxb object factory (Defaults to the static one but can be
     *  changed). */
    private ObjectFactory mJaxbObjectFactory = JF;
    
    /**
     * Constructor for a root Complex element without a bound JAXB object.
     */
    public <xsl:value-of select="$binding-class-name"/>() {
        this(null);
    }

    /**
     * Constructor for a root Complex element with a bound JAXB object.
     * 
     * @param jaxbObject the concrete JAXB object instance bound to this
     *        complex element
     */
    public <xsl:value-of select="$binding-class-name"/>(
            final <xsl:value-of select="@jaxb-type"/> jaxbObject) {
        this("<xsl:value-of select="@binding-name"/>", "<xsl:value-of select="@jaxb-name"/>", null, jaxbObject);
    }

     /**
     * Constructor for a Complex element as a child of another element and
     * an associated JAXB object.
     * 
     * @param name the identifier for this binding
     * @param jaxbName name of field in parent JAXB object
     * @param jaxbObject the concrete JAXB object instance bound to this
     *        complex element
     * @param parentBinding a reference to the parent binding
     */
    public <xsl:value-of select="$binding-class-name"/>(
            final String name,
            final String jaxbName,
            final ICobolComplexBinding parentBinding,
            final <xsl:value-of select="@jaxb-type"/> jaxbObject) {
        
        super(name, jaxbName, <xsl:value-of select="@jaxb-type"/>.class, null, parentBinding);
        mJaxbObject = jaxbObject;
        if (mJaxbObject != null) {
            mUnusedJaxbObject = true;
        }
        initChildren();
    }

    /** Creates a binding property for each child. */
    private void initChildren() {
        if (LOG.isDebugEnabled()) {
            LOG.debug("Initializing started");
        }
        /* Create binding children instances */
        <xsl:apply-templates select="coxb-property" mode="generate-property-init">
             <xsl:with-param name="parent-binding">this</xsl:with-param>
        </xsl:apply-templates>
        /* Add children to children list */<xsl:for-each select="coxb-property">
        getChildrenList().add(<xsl:value-of select="@var-name"/>);</xsl:for-each>

        if (LOG.isDebugEnabled()) {
            LOG.debug("Initializing successful");
        }
    }

    /** {@inheritDoc} */
    public final void createJaxbObject() throws HostException {
        /* Since this complex binding has a constructor that takes a
         * JAXB object, we might already have a Jaxb object that
         * was not used yet. */
        if (mUnusedJaxbObject &amp;&amp; mJaxbObject != null) {
            mUnusedJaxbObject = false;
            return;
        }
        mJaxbObject = mJaxbObjectFactory.create<xsl:value-of select="@jaxb-type"/>();
    }

    /** {@inheritDoc} */
    public final void setChildrenValues() throws HostException {

         /* Make sure there is an associated JAXB object*/
        if (mJaxbObject == null) {
            createJaxbObject();
        }
        <xsl:for-each select="coxb-property[@jaxb-name != 'null' and @jaxb-type != 'null']">
            <xsl:call-template name="generate-get-values-from-jaxb"/>
        </xsl:for-each>
    }

    /** {@inheritDoc} */
    @SuppressWarnings("unchecked")
    public final void setJaxbPropertyValue(final int index) throws HostException {
 
        ICobolBinding child = getChildrenList().get(index);
        
    	/* Children that are not bound to a jaxb property are ignored.
    	 * This includes Choices and dynamically generated counbters
    	 * for instance.  */
        if (!child.isBound()) {
            return;
        }
        
        Object value = child.getObjectValue(child.getJaxbType());
        if (LOG.isDebugEnabled()) {
            LOG.debug("Setting value of JAXB property "
                    + child.getJaxbName()
                    + " value=" + value);
        }
        /* Set the JAXB object property value from binding object */
        switch (index) {<xsl:for-each select="coxb-property">
        case <xsl:value-of select="position() - 1"/>:<xsl:if test="@jaxb-name != 'null' and @jaxb-type != 'null'">
       <xsl:choose>
            <xsl:when test="@cobol-maxOccurs > 0 or @type = 'complexArray'">
            mJaxbObject.<xsl:value-of select="concat('get',upper-case(substring(@jaxb-name,1,1)),substring(@jaxb-name,2))"/>().clear();
            mJaxbObject.<xsl:value-of select="concat('get',upper-case(substring(@jaxb-name,1,1)),substring(@jaxb-name,2))"/>().addAll(
                (List &lt; <xsl:value-of select="@jaxb-type"/> &gt;) value);</xsl:when><xsl:otherwise>
            mJaxbObject.<xsl:value-of select="concat('set',upper-case(substring(@jaxb-name,1,1)),substring(@jaxb-name,2))"/>((<xsl:value-of select="@jaxb-type"/>) value);</xsl:otherwise>
       </xsl:choose>
       </xsl:if>
            break;</xsl:for-each>
        default:
            break;
        }
    }
            <xsl:apply-templates select="coxb-property" mode="generate-set-jaxb-values"/>
    /** {@inheritDoc} */
    public final Object getObjectValue(final Class type) throws HostException {
        if (type.equals(<xsl:value-of select="@jaxb-type"/>.class)) {
            return mJaxbObject;
        } else {
            throw new HostException("Attempt to get binding " + getBindingName()
                    + " as an incompatible type " + type);
        }
    }

    /** {@inheritDoc} */
    public final void setObjectValue(final Object value) throws HostException {
        if (value == null) {
            mJaxbObject = null;
            return;
        }
        if (value.getClass().equals(<xsl:value-of select="@jaxb-type"/>.class)) {
            mJaxbObject = (<xsl:value-of select="@jaxb-type"/>) value;
        } else {
            throw new HostException("Attempt to set binding " + getBindingName()
                    + " from an incompatible value " + value);
        }
    }

    /**
     * @return the java object factory for objects creation
     */
    public final ObjectFactory getObjectFactory() {
        return mJaxbObjectFactory;
    }

    /**
     * @param jaxbObjectFactory the java object factory for objects creation 
     */
    public final void setObjectFactory(final Object jaxbObjectFactory) {
        mJaxbObjectFactory = (ObjectFactory) jaxbObjectFactory;
    }

    /** {@inheritDoc} */
    public final boolean isSet() {
        return (mJaxbObject != null);
    }

    /**
     * @return the bound JAXB object
     */
    public final <xsl:value-of select="@jaxb-type"/> get<xsl:value-of select="@jaxb-type"/>() {
        return mJaxbObject;
    }
    
}
</xsl:template>

<!-- ===============================================================================================
   Generate the code of the java class that implements a choice binding
 -->
<xsl:template name="generate-choice-class">
<xsl:param name="binding-class-name"/>
 * Represents a choice between 2 or more elements. A choice maps to a cobol
 * REDEFINES clause exposed as an xs:choice in the corresponding XML schema
 */

public class <xsl:value-of select="$binding-class-name"/> 
             extends CChoiceBinding {

    /** Alternatives of this choice binding. */
    <xsl:for-each select="coxb-property">
    /** Alternative bound to jaxb property <xsl:value-of select="@jaxb-name"/>(<xsl:value-of select="@jaxb-type"/>). */
    public <xsl:value-of select="@binding-type"/><xsl:text> </xsl:text><xsl:value-of select="@var-name"/>;</xsl:for-each>

    /** Logger. */
    private static final Log LOG =
        LogFactory.getLog(<xsl:value-of select="$binding-class-name"/>.class);

    <xsl:if test="count(coxb-property[@type = 'simple']) > 0">
    /** Binding factory. */
    private static final ICobolBindingFactory BF
        = CobolBindingFactory.getBindingFactory();</xsl:if>
    
    /**
     * Constructor for a Choice element.
     * 
     * @param name the identifier for this binding
     * @param parentBinding a reference to the parent binding
     */
    public <xsl:value-of select="$binding-class-name"/>(
            final String name,
            final ICobolComplexBinding parentBinding) {
        
		super(name, null, parentBinding);
        setMarshalChoiceStrategyClassName("<xsl:value-of select="@marshalChoiceStrategyClassName"/>");
        setUnmarshalChoiceStrategyClassName("<xsl:value-of select="@unmarshalChoiceStrategyClassName"/>");
        initAlternatives();
    }

    /** Creates a binding property for each alternative. */
    private void initAlternatives() {
        if (LOG.isDebugEnabled()) {
            LOG.debug("Initializing started");
        }
        /* Create binding alternatives instances */
        <xsl:apply-templates select="coxb-property" mode="generate-property-init">
             <xsl:with-param name="parent-binding">getParentBinding()</xsl:with-param>
        </xsl:apply-templates>
        /* Add alternatives to alternatives list */<xsl:for-each select="coxb-property">
        addAlternative(<xsl:value-of select="@var-name"/>);</xsl:for-each>

        if (LOG.isDebugEnabled()) {
            LOG.debug("Initializing successful");
        }
    }
 
    /** {@inheritDoc} */
    public final void setAlternativesValues() throws HostException {
        Object value;
    <xsl:for-each select="coxb-property[@type != 'choice']">
        value = get<xsl:value-of select="../@parent-jaxb-type"/>().get<xsl:value-of select="@jaxb-name"/>();
        if (value != null) {
            if (LOG.isDebugEnabled()) {
                LOG.debug("Getting value from JAXB property "
                        + "<xsl:value-of select="@jaxb-name"/>"
                        + " value=" + value);
            }
            <xsl:value-of select="@var-name"/>.setObjectValue(value);
        }
    </xsl:for-each>
    }
    /** {@inheritDoc} */
    public final void setJaxbPropertyValue(
            final int index) throws HostException {
        /* Set the JAXB object property value from binding object */
        ICobolBinding alt = getAlternativesList().get(index);

        /* Choice children are a special case. They directly set 
         * their parent object depending on the chosen choice
         * strategy. */
        if (alt instanceof ICobolChoiceBinding) {
            return;
        }

        Object value = alt.getObjectValue(alt.getJaxbType());
        if (LOG.isDebugEnabled()) {
            LOG.debug("Setting value of JAXB property "
                    + alt.getJaxbName()
                    + " value=" + value);
        }
        switch (index) {<xsl:for-each select="coxb-property">
        case <xsl:value-of select="position() - 1"/>:
            get<xsl:value-of select="../@parent-jaxb-type"/>().<xsl:value-of select="concat('set',upper-case(substring(@jaxb-name,1,1)),substring(@jaxb-name,2))"/>(
                (<xsl:value-of select="@jaxb-type"/>) <xsl:value-of select="@var-name"/>.getObjectValue(<xsl:value-of select="@jaxb-type"/>.class));
            break;</xsl:for-each>
        default:
            break;
        }
    }
    
    /** {@inheritDoc} */
    public final Object getObjectValue(final Class type) throws HostException {
        throw new HostException("Attempt to get value from choice binding "
                + getCobolName());
    }

    /** {@inheritDoc} */
    public final void setObjectValue(final Object value) throws HostException {
        throw new HostException("Attempt to set value for choice binding "
                + getCobolName());
    }
    
    /** {@inheritDoc} */
    public final boolean isSet() {
        /* A Choice is considered set if at least one of its alternatives
         * is set. */
        for (ICobolBinding alt : getAlternativesList()) {
            if (alt.isSet()) {
                return true;
            }
        }
        return false;
    }

    /**
     * @return the JAXB object bound to the parent of this choice
     * @throws HostException if parent bound JAXB object cannot be retrieved
     */
    public final <xsl:value-of select="@parent-jaxb-type"/> get<xsl:value-of select="@parent-jaxb-type"/>() throws HostException {
        return (<xsl:value-of select="@parent-jaxb-type"/>) getParentJaxbObject();
    }

}
</xsl:template>

<!-- ===============================================================================================
   Generate the code of the java class that implements a complex array binding
 -->
<xsl:template name="generate-complex-array-class">
<xsl:param name="binding-class-name"/>
<xsl:variable name="jaxbobject-class"><xsl:value-of select="concat('List &lt; ',@item-jaxb-type,' &gt;')"/></xsl:variable>
 * Represents an array of complex (record) elements. A complex array maps to
 * a cobol OCCURS of group items and to java Lists.
 */

public class <xsl:value-of select="$binding-class-name"/> 
             extends CArrayComplexBinding {

    /** Java object to which this cobol complex array element is bound. */
    private <xsl:value-of select="$jaxbobject-class"/> mJaxbObject;
    
    /** Logger. */
    private static final Log LOG
        = LogFactory.getLog(<xsl:value-of select="$binding-class-name"/>.class);

    /**
     * Constructor for an array of Complex elements.
     * 
     * @param name the identifier for this binding
     * @param jaxbName name of field in parent JAXB object
     * @param parentBinding a reference to the parent binding
     * @param complexItemBinding a binding element for array items
     */
    public <xsl:value-of select="$binding-class-name"/>(
            final String name,
            final String jaxbName,
            final ICobolComplexBinding parentBinding,
            final ICobolComplexBinding complexItemBinding) {
        
        super(name, jaxbName, <xsl:value-of select="@item-jaxb-type"/>.class, null, parentBinding, complexItemBinding);
        setMinOccurs(<xsl:value-of select="@cobol-minOccurs"/>);
        setMaxOccurs(<xsl:value-of select="@cobol-maxOccurs"/>);<xsl:if test="@cobol-dependingOn">
        setDependingOn("<xsl:value-of select="@cobol-dependingOn"/>");
        </xsl:if>
    }

    /** {@inheritDoc} */
    public final void createJaxbObject() throws HostException {
        mJaxbObject = new ArrayList &lt; <xsl:value-of select="@item-jaxb-type"/> &gt;();
    }

    /** {@inheritDoc} */
    public final void setItemValue(
        final int index) throws HostException {
         /* Make sure there is an associated JAXB object*/
        if (mJaxbObject == null) {
            createJaxbObject();
        }
        /* The Jaxb list might have less items than expected by the binding.
         * In this case, we fill the binding with empty items. */
        if (index &lt; mJaxbObject.size()) {
            if (LOG.isDebugEnabled()) {
                LOG.debug("Getting value from item " +  index
                        + " of JAXB property "
                        + "<xsl:value-of select="$jaxbobject-class"/>"
                        + " value=" + mJaxbObject.get(index));
            }
            getComplexItemBinding().setObjectValue(mJaxbObject.get(index));
        } else {
            if (LOG.isDebugEnabled()) {
                LOG.debug("Initializing item " +  index);
            }
            getComplexItemBinding().setObjectValue(null);
        }
    }

    /** {@inheritDoc} */
    @SuppressWarnings("unchecked")
    public final void addJaxbPropertyValue(
        final int index) throws HostException {
         /* Make sure there is an associated JAXB object*/
        if (mJaxbObject == null) {
            throw new HostException(
                    "Binded object not initialized for " + getBindingName());
        }
        mJaxbObject.add((<xsl:value-of select="@item-jaxb-type"/>) getComplexItemBinding().
        		getObjectValue(<xsl:value-of select="@item-jaxb-type"/>.class));
    }
 
    /** {@inheritDoc} */
    public final List getObjectList() {
        return mJaxbObject;
    }

    /** {@inheritDoc} */
    @SuppressWarnings("unchecked")
    public final void setObjectList(
            final List list) {
        mJaxbObject = list;
    }
    
    /** {@inheritDoc} */
    public final Object getObjectValue(final Class type) throws HostException {
        if (type.equals(<xsl:value-of select="@item-jaxb-type"/>.class)) {
            return mJaxbObject;
        } else {
            throw new HostException("Attempt to get binding " + getBindingName()
                    + " as an incompatible type " + type);
        }
    }

    /** {@inheritDoc} */
     @SuppressWarnings("unchecked")
    public final void setObjectValue(final Object value) throws HostException {
        if (value == null) {
            mJaxbObject = null;
            return;
        }
        if (value instanceof List) {
            if (((List) value).size() == 0) {
                mJaxbObject = new ArrayList &lt; <xsl:value-of select="@item-jaxb-type"/> &gt;();
                return;
            }
            /* We assume all items will have the same type as the first one.
             * The unchecked cast might break at runtime. */
            Object item = ((List) value).get(0);
            if (item.getClass().equals(<xsl:value-of select="@item-jaxb-type"/>.class)) {
                mJaxbObject = (List &lt; <xsl:value-of select="@item-jaxb-type"/> &gt;) value;
                return;
            }
        }
        throw new HostException("Attempt to set binding " + getBindingName()
                + " from an incompatible value " + value);
    }

    /** {@inheritDoc} */
    public final boolean isSet() {
        return (mJaxbObject != null);
    }

    /**
     * @return the bound JAXB object
     * @throws HostException if bound JAXB object cannot be retrieved
     */
    @SuppressWarnings("unchecked")
    public final List &lt; <xsl:value-of select="@item-jaxb-type"/> &gt; get<xsl:value-of select="@item-jaxb-type"/>() throws HostException {
        return (List &lt; <xsl:value-of select="@item-jaxb-type"/> &gt;) getObjectValue(<xsl:value-of select="@item-jaxb-type"/>.class);
    }
    
}
</xsl:template>

<!-- ===============================================================================================
   Generate the code to initialize individual properties
 -->
<xsl:template match="coxb-property" mode="generate-property-init">
<xsl:param name="parent-binding"/>
  <xsl:choose>
    <xsl:when test="@type = 'simple'">
        <xsl:value-of select="@var-name"/> = BF.create<xsl:value-of select="substring-after(@binding-type, 'ICobol')"/>("<xsl:value-of select="@binding-name"/>", <xsl:choose><xsl:when test="@jaxb-name = 'null' and @jaxb-type = 'null'">
               <xsl:value-of select="$parent-binding"/>);
        </xsl:when><xsl:otherwise>
               "<xsl:value-of select="@jaxb-name"/>", <xsl:value-of select="@jaxb-type"/>.class, <xsl:value-of select="$parent-binding"/>);
        </xsl:otherwise>
        </xsl:choose>
    </xsl:when>
    <xsl:when test="@type = 'choice'">
        <xsl:value-of select="@var-name"/> = new <xsl:value-of select="@binding-type"/>("<xsl:value-of select="@binding-name"/>", <xsl:value-of select="$parent-binding"/>);
        </xsl:when>
    <xsl:when test="@type = 'complex'">
    <xsl:value-of select="@var-name"/> = new <xsl:value-of select="@binding-type"/>("<xsl:value-of select="@binding-name"/>",
             "<xsl:value-of select="@jaxb-name"/>", <xsl:value-of select="$parent-binding"/>, null);
        </xsl:when>
    <xsl:when test="@type = 'complexArray'">
    <xsl:value-of select="@var-name"/>Item = new <xsl:value-of select="@item-binding-type"/>("<xsl:value-of select="@binding-name"/>Item",
             "<xsl:value-of select="@jaxb-name"/>", <xsl:value-of select="$parent-binding"/>, null);
        <xsl:value-of select="@var-name"/> = new <xsl:value-of select="@binding-type"/>("<xsl:value-of select="@binding-name"/>",
             "<xsl:value-of select="@jaxb-name"/>", <xsl:value-of select="$parent-binding"/>, <xsl:value-of select="@var-name"/>Item);
        </xsl:when>
  </xsl:choose>

  <xsl:if test="@cobol-byteLength">
    <xsl:value-of select="@var-name"/>.setByteLength(<xsl:value-of select="@cobol-byteLength"/>);
        </xsl:if>
  <xsl:if test="@cobol-name">
    <xsl:value-of select="@var-name"/>.setCobolName("<xsl:value-of select="@cobol-name"/>");
        </xsl:if>
  <xsl:if test="@cobol-isJustifiedRight = 'true'">
    <xsl:value-of select="@var-name"/>.setIsJustifiedRight(true);
        </xsl:if>
  <xsl:if test="@cobol-totalDigits > 0">
    <xsl:value-of select="@var-name"/>.setTotalDigits(<xsl:value-of select="@cobol-totalDigits"/>);
        </xsl:if>
  <xsl:if test="@cobol-fractionDigits > 0">
    <xsl:value-of select="@var-name"/>.setFractionDigits(<xsl:value-of select="@cobol-fractionDigits"/>);
        </xsl:if>
  <xsl:if test="@cobol-isSigned = 'true'">
    <xsl:value-of select="@var-name"/>.setIsSigned(true);
        </xsl:if>
  <xsl:if test="@cobol-isSignLeading = 'true'">
    <xsl:value-of select="@var-name"/>.setIsSignLeading(true);
        </xsl:if>
  <xsl:if test="@cobol-isSignSeparate = 'true'">
    <xsl:value-of select="@var-name"/>.setIsSignSeparate(true);
        </xsl:if>
  <xsl:if test="@cobol-minOccurs > 0">
    <xsl:value-of select="@var-name"/>.setMinOccurs(<xsl:value-of select="@cobol-minOccurs"/>);
        </xsl:if>
  <xsl:if test="@cobol-maxOccurs > 0">
    <xsl:value-of select="@var-name"/>.setMaxOccurs(<xsl:value-of select="@cobol-maxOccurs"/>);
        </xsl:if>
  <xsl:if test="@cobol-redefines">
    <xsl:value-of select="@var-name"/>.setRedefines("<xsl:value-of select="@cobol-redefines"/>");
        </xsl:if>
  <xsl:if test="@cobol-isODOObject = 'true'">
    <xsl:value-of select="@var-name"/>.setIsODOObject(true);
        </xsl:if>
  <xsl:if test="@cobol-dependingOn">
    <xsl:value-of select="@var-name"/>.setDependingOn("<xsl:value-of select="@cobol-dependingOn"/>");
        </xsl:if>
  <xsl:if test="@cobol-isCustomVariable">
    <xsl:value-of select="@var-name"/>.setIsCustomVariable(<xsl:value-of select="@cobol-isCustomVariable"/>);
        </xsl:if>
  <xsl:if test="@marshalChoiceStrategyClassName">
    <xsl:value-of select="@var-name"/>.setMarshalChoiceStrategyClassName("<xsl:value-of select="@marshalChoiceStrategyClassName"/>");
        </xsl:if>
  <xsl:if test="@unmarshalChoiceStrategyClassName">
    <xsl:value-of select="@var-name"/>.setUnmarshalChoiceStrategyClassName("<xsl:value-of select="@unmarshalChoiceStrategyClassName"/>");
        </xsl:if>
  
</xsl:template>

<!-- ===============================================================================================
   Generate the code to set binding values from JAXB instance properties
 -->
<xsl:template name="generate-get-values-from-jaxb">
    <xsl:variable name="jaxb-getter-method">
      <xsl:value-of select="concat('get',upper-case(substring(@jaxb-name,1,1)),substring(@jaxb-name,2))"/>
    </xsl:variable>
        /* Get JAXB property <xsl:value-of select="@jaxb-name"/> */
        if (LOG.isDebugEnabled()) {
            LOG.debug("Getting value from JAXB property "
                    + "<xsl:value-of select="@jaxb-name"/>"
                    + " value=" + mJaxbObject.<xsl:value-of select="$jaxb-getter-method"/>());
        }
        <xsl:value-of select="@var-name"/>.setObjectValue(mJaxbObject.<xsl:value-of select="$jaxb-getter-method"/>());
        <xsl:if test="@cobol-maxOccurs &gt; 1 and @cobol-minOccurs &lt; @cobol-maxOccurs">
        /* For variable size array or list, we make sure any
         * associated counter is updated */
        setCounterValue(<xsl:value-of select="@var-name"/>.getDependingOn(),
                ((List) mJaxbObject.<xsl:value-of select="$jaxb-getter-method"/>()).size());
        </xsl:if>
  
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
            <xsl:value-of select="concat(@jaxb-package,'.bind')"/>
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
import com.legstar.coxb.host.HostException;
import <xsl:value-of select="concat(@jaxb-package, '.', @parent-jaxb-type)"/>;

/** 
 * Skeleton implementation of a custom choice selection strategy. Modify this
 * code to select a suitable alternative.
 */
public class <xsl:value-of select="$choice-strategy-class-name"/> implements ICobolUnmarshalChoiceStrategy {

    /** {@inheritDoc} */
    public final ICobolBinding choose(
        final ICobolChoiceBinding choice,
        final Hashtable &lt; String, Object &gt; variablesMap,
        final CobolElementVisitor visitor) throws HostException {
        
        /* Get the parent JAXB object which properties might help select the
         * right alternative. */
        <xsl:value-of select="@parent-jaxb-type"/> jaxbo = (<xsl:value-of select="@parent-jaxb-type"/>) choice.getObjectValue(<xsl:value-of select="@parent-jaxb-type"/>.class);
        assert (jaxbo != null);
        
        /* Replace following code with actual logic. */
        int index = 0;
        switch (index) {<xsl:for-each select="coxb-property">
        case <xsl:value-of select="position() - 1"/>:
            return choice.getAlternativeByName("<xsl:value-of select="@binding-name"/>");</xsl:for-each>
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

/*******************************************************************************
 * Copyright (c) 2011 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.coxb.impl.visitor;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolElementVisitor;
import com.legstar.coxb.CobolType;
import com.legstar.coxb.ICobolBinding;
import com.legstar.coxb.ICobolChoiceBinding;
import com.legstar.coxb.ICobolUnmarshalChoiceStrategy;
import com.legstar.coxb.host.HostException;

import junit.framework.TestCase;

/**
 * A set of JAXB objects that are useful for visitors unit testing.
 *
 */
public abstract class AbstractVisitorTest extends TestCase {

    /**
     * A flat simple structure.
     *
     */
    @XmlAccessorType(XmlAccessType.FIELD)
    @XmlType(name = "Flat01Record", propOrder = {
        "comNumber",
        "comName",
        "comAmount"
    })
    public class Flat01Record {

        @CobolElement(cobolName = "COM-NUMBER", type = CobolType.ZONED_DECIMAL_ITEM, levelNumber = 5, isSigned = false, totalDigits = 6, picture = "9(6)", srceLine = 25)
        protected long comNumber;
        @CobolElement(cobolName = "COM-NAME", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, picture = "X(20)", srceLine = 26)
        protected String comName;
        @CobolElement(cobolName = "COM-AMOUNT", type = CobolType.PACKED_DECIMAL_ITEM, levelNumber = 5, isSigned = false, totalDigits = 7, fractionDigits = 2, picture = "9(5)V99", usage = "PACKED-DECIMAL", srceLine = 27)
        protected BigDecimal comAmount;

        /**
         * Gets the value of the comNumber property.
         * 
         */
        public long getComNumber() {
            return comNumber;
        }

        /**
         * Sets the value of the comNumber property.
         * 
         */
        public void setComNumber(long value) {
            this.comNumber = value;
        }

        public boolean isSetComNumber() {
            return true;
        }

        /**
         * Gets the value of the comName property.
         * 
         * @return possible object is {@link String }
         * 
         */
        public String getComName() {
            return comName;
        }

        /**
         * Sets the value of the comName property.
         * 
         * @param value allowed object is {@link String }
         * 
         */
        public void setComName(String value) {
            this.comName = value;
        }

        public boolean isSetComName() {
            return (this.comName != null);
        }

        /**
         * Gets the value of the comAmount property.
         * 
         * @return possible object is {@link BigDecimal }
         * 
         */
        public BigDecimal getComAmount() {
            return comAmount;
        }

        /**
         * Sets the value of the comAmount property.
         * 
         * @param value allowed object is {@link BigDecimal }
         * 
         */
        public void setComAmount(BigDecimal value) {
            this.comAmount = value;
        }

        public boolean isSetComAmount() {
            return (this.comAmount != null);
        }

    }

    /**
     * A mock JAXB ObjectFactory.
     *
     */
    public class Flat01RecordFactory {

        /**
         * Create an instance of {@link Flat01Record }
         * 
         */
        public Flat01Record createAbstractVisitorTest$Flat01Record() {
            return new Flat01Record();
        }

    }

    /**
     * A flat structure with an array.
     *
     */
    @XmlAccessorType(XmlAccessType.FIELD)
    @XmlType(name = "Flat02Record", propOrder = {
        "comNumber",
        "comName",
        "comAmount",
        "comArray"
    })
    public class Flat02Record
        implements Serializable
    {

        private final static long serialVersionUID = 1L;
        @CobolElement(cobolName = "COM-NUMBER", type = CobolType.ZONED_DECIMAL_ITEM, levelNumber = 5, isSigned = false, totalDigits = 6, picture = "9(6)", srceLine = 2)
        protected long comNumber;
        @XmlElement(required = true)
        @CobolElement(cobolName = "COM-NAME", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, picture = "X(20)", srceLine = 3)
        protected String comName;
        @XmlElement(required = true)
        @CobolElement(cobolName = "COM-AMOUNT", type = CobolType.PACKED_DECIMAL_ITEM, levelNumber = 5, isSigned = false, totalDigits = 7, fractionDigits = 2, picture = "9(5)V99", usage = "PACKED-DECIMAL", srceLine = 4)
        protected BigDecimal comAmount;
        @XmlElement(type = Short.class)
        @CobolElement(cobolName = "COM-ARRAY", type = CobolType.BINARY_ITEM, levelNumber = 5, isSigned = true, totalDigits = 4, minOccurs = 5, maxOccurs = 5, picture = "S9(4)", usage = "BINARY", srceLine = 5)
        protected List<Short> comArray;

        /**
         * Gets the value of the comNumber property.
         * 
         */
        public long getComNumber() {
            return comNumber;
        }

        /**
         * Sets the value of the comNumber property.
         * 
         */
        public void setComNumber(long value) {
            this.comNumber = value;
        }

        public boolean isSetComNumber() {
            return true;
        }

        /**
         * Gets the value of the comName property.
         * 
         * @return
         *     possible object is
         *     {@link String }
         *     
         */
        public String getComName() {
            return comName;
        }

        /**
         * Sets the value of the comName property.
         * 
         * @param value
         *     allowed object is
         *     {@link String }
         *     
         */
        public void setComName(String value) {
            this.comName = value;
        }

        public boolean isSetComName() {
            return (this.comName!= null);
        }

        /**
         * Gets the value of the comAmount property.
         * 
         * @return
         *     possible object is
         *     {@link BigDecimal }
         *     
         */
        public BigDecimal getComAmount() {
            return comAmount;
        }

        /**
         * Sets the value of the comAmount property.
         * 
         * @param value
         *     allowed object is
         *     {@link BigDecimal }
         *     
         */
        public void setComAmount(BigDecimal value) {
            this.comAmount = value;
        }

        public boolean isSetComAmount() {
            return (this.comAmount!= null);
        }

        /**
         * Gets the value of the comArray property.
         * 
         * <p>
         * This accessor method returns a reference to the live list,
         * not a snapshot. Therefore any modification you make to the
         * returned list will be present inside the JAXB object.
         * This is why there is not a <CODE>set</CODE> method for the comArray property.
         * 
         * <p>
         * For example, to add a new item, do as follows:
         * <pre>
         *    getComArray().add(newItem);
         * </pre>
         * 
         * 
         * <p>
         * Objects of the following type(s) are allowed in the list
         * {@link Short }
         * 
         * 
         */
        public List<Short> getComArray() {
            if (comArray == null) {
                comArray = new ArrayList<Short>();
            }
            return this.comArray;
        }

        public boolean isSetComArray() {
            return ((this.comArray!= null)&&(!this.comArray.isEmpty()));
        }

        public void unsetComArray() {
            this.comArray = null;
        }

    }

    /**
     * A mock JAXB ObjectFactory.
     *
     */
    public class Flat02RecordFactory {

        /**
         * Create an instance of {@link Flat02Record }
         * 
         */
        public Flat02Record createAbstractVisitorTest$Flat02Record() {
            return new Flat02Record();
        }

    }

    /**
     * A structure with a complex array.
     *
     */
    @XmlAccessorType(XmlAccessType.FIELD)
    @XmlType(name = "Stru03Record", propOrder = {
        "comNumber",
        "comName",
        "comAmount",
        "comArray"
    })
    public class Stru03Record
        implements Serializable
    {

        private final static long serialVersionUID = 1L;
        @CobolElement(cobolName = "COM-NUMBER", type = CobolType.ZONED_DECIMAL_ITEM, levelNumber = 5, isSigned = false, totalDigits = 6, picture = "9(6)", srceLine = 2)
        protected long comNumber;
        @XmlElement(required = true)
        @CobolElement(cobolName = "COM-NAME", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, picture = "X(20)", srceLine = 3)
        protected String comName;
        @XmlElement(required = true)
        @CobolElement(cobolName = "COM-AMOUNT", type = CobolType.PACKED_DECIMAL_ITEM, levelNumber = 5, isSigned = false, totalDigits = 7, fractionDigits = 2, picture = "9(5)V99", usage = "PACKED-DECIMAL", srceLine = 4)
        protected BigDecimal comAmount;
        @XmlElement(required = true)
        @CobolElement(cobolName = "COM-ARRAY", type = CobolType.GROUP_ITEM, levelNumber = 5, minOccurs = 5, maxOccurs = 5, srceLine = 5)
        protected List<ComArray> comArray;

        /**
         * Gets the value of the comNumber property.
         * 
         */
        public long getComNumber() {
            return comNumber;
        }

        /**
         * Sets the value of the comNumber property.
         * 
         */
        public void setComNumber(long value) {
            this.comNumber = value;
        }

        public boolean isSetComNumber() {
            return true;
        }

        /**
         * Gets the value of the comName property.
         * 
         * @return
         *     possible object is
         *     {@link String }
         *     
         */
        public String getComName() {
            return comName;
        }

        /**
         * Sets the value of the comName property.
         * 
         * @param value
         *     allowed object is
         *     {@link String }
         *     
         */
        public void setComName(String value) {
            this.comName = value;
        }

        public boolean isSetComName() {
            return (this.comName!= null);
        }

        /**
         * Gets the value of the comAmount property.
         * 
         * @return
         *     possible object is
         *     {@link BigDecimal }
         *     
         */
        public BigDecimal getComAmount() {
            return comAmount;
        }

        /**
         * Sets the value of the comAmount property.
         * 
         * @param value
         *     allowed object is
         *     {@link BigDecimal }
         *     
         */
        public void setComAmount(BigDecimal value) {
            this.comAmount = value;
        }

        public boolean isSetComAmount() {
            return (this.comAmount!= null);
        }

        /**
         * Gets the value of the comArray property.
         * 
         * <p>
         * This accessor method returns a reference to the live list,
         * not a snapshot. Therefore any modification you make to the
         * returned list will be present inside the JAXB object.
         * This is why there is not a <CODE>set</CODE> method for the comArray property.
         * 
         * <p>
         * For example, to add a new item, do as follows:
         * <pre>
         *    getComArray().add(newItem);
         * </pre>
         * 
         * 
         * <p>
         * Objects of the following type(s) are allowed in the list
         * {@link ComArray }
         * 
         * 
         */
        public List<ComArray> getComArray() {
            if (comArray == null) {
                comArray = new ArrayList<ComArray>();
            }
            return this.comArray;
        }

        public boolean isSetComArray() {
            return ((this.comArray!= null)&&(!this.comArray.isEmpty()));
        }

        public void unsetComArray() {
            this.comArray = null;
        }

    }

    /**
     * A complex array item.
     *
     */
    @XmlAccessorType(XmlAccessType.FIELD)
    @XmlType(name = "ComArray", propOrder = {
        "comItem1",
        "comItem2"
    })
    public class ComArray
        implements Serializable
    {

        private final static long serialVersionUID = 1L;
        @CobolElement(cobolName = "COM-ITEM1", type = CobolType.BINARY_ITEM, levelNumber = 10, isSigned = true, totalDigits = 4, picture = "S9(4)", usage = "BINARY", srceLine = 6)
        protected short comItem1;
        @XmlElement(required = true)
        @CobolElement(cobolName = "COM-ITEM2", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 10, picture = "X(2)", srceLine = 7)
        protected String comItem2;

        /**
         * Gets the value of the comItem1 property.
         * 
         */
        public short getComItem1() {
            return comItem1;
        }

        /**
         * Sets the value of the comItem1 property.
         * 
         */
        public void setComItem1(short value) {
            this.comItem1 = value;
        }

        public boolean isSetComItem1() {
            return true;
        }

        /**
         * Gets the value of the comItem2 property.
         * 
         * @return
         *     possible object is
         *     {@link String }
         *     
         */
        public String getComItem2() {
            return comItem2;
        }

        /**
         * Sets the value of the comItem2 property.
         * 
         * @param value
         *     allowed object is
         *     {@link String }
         *     
         */
        public void setComItem2(String value) {
            this.comItem2 = value;
        }

        public boolean isSetComItem2() {
            return (this.comItem2 != null);
        }

    }

    /**
     * A mock JAXB ObjectFactory.
     *
     */
    public class Stru03RecordFactory {

        /**
         * Create an instance of {@link Stru03Record }
         * 
         */
        public Stru03Record createAbstractVisitorTest$Stru03Record() {
            return new Stru03Record();
        }

        /**
         * Create an instance of {@link ComArray }
         * 
         */
        public ComArray createAbstractVisitorTest$ComArray() {
            return new ComArray();
        }
    }

    /**
     * A complex structure with complex arrays.
     *
     */
    @XmlAccessorType(XmlAccessType.FIELD)
    @XmlType(name = "Stru04Record", propOrder = {
        "comItem1",
        "comArray1",
        "comItem8"
    })
    public class Stru04Record
        implements Serializable
    {

        private final static long serialVersionUID = 1L;
        @XmlElement(required = true)
        @CobolElement(cobolName = "COM-ITEM1", type = CobolType.PACKED_DECIMAL_ITEM, levelNumber = 5, isSigned = false, totalDigits = 7, fractionDigits = 2, picture = "9(5)V99", usage = "PACKED-DECIMAL", srceLine = 2)
        protected BigDecimal comItem1;
        @XmlElement(required = true)
        @CobolElement(cobolName = "COM-ARRAY1", type = CobolType.GROUP_ITEM, levelNumber = 5, minOccurs = 3, maxOccurs = 3, srceLine = 3)
        protected List<ComArray1> comArray1;
        @XmlElement(required = true)
        @CobolElement(cobolName = "COM-ITEM8", type = CobolType.PACKED_DECIMAL_ITEM, levelNumber = 5, isSigned = false, totalDigits = 7, fractionDigits = 2, picture = "9(5)V99", usage = "PACKED-DECIMAL", srceLine = 13)
        protected BigDecimal comItem8;

        /**
         * Gets the value of the comItem1 property.
         * 
         * @return
         *     possible object is
         *     {@link BigDecimal }
         *     
         */
        public BigDecimal getComItem1() {
            return comItem1;
        }

        /**
         * Sets the value of the comItem1 property.
         * 
         * @param value
         *     allowed object is
         *     {@link BigDecimal }
         *     
         */
        public void setComItem1(BigDecimal value) {
            this.comItem1 = value;
        }

        public boolean isSetComItem1() {
            return (this.comItem1 != null);
        }

        /**
         * Gets the value of the comArray1 property.
         * 
         * <p>
         * This accessor method returns a reference to the live list,
         * not a snapshot. Therefore any modification you make to the
         * returned list will be present inside the JAXB object.
         * This is why there is not a <CODE>set</CODE> method for the comArray1 property.
         * 
         * <p>
         * For example, to add a new item, do as follows:
         * <pre>
         *    getComArray1().add(newItem);
         * </pre>
         * 
         * 
         * <p>
         * Objects of the following type(s) are allowed in the list
         * {@link ComArray1 }
         * 
         * 
         */
        public List<ComArray1> getComArray1() {
            if (comArray1 == null) {
                comArray1 = new ArrayList<ComArray1>();
            }
            return this.comArray1;
        }

        public boolean isSetComArray1() {
            return ((this.comArray1 != null)&&(!this.comArray1 .isEmpty()));
        }

        public void unsetComArray1() {
            this.comArray1 = null;
        }

        /**
         * Gets the value of the comItem8 property.
         * 
         * @return
         *     possible object is
         *     {@link BigDecimal }
         *     
         */
        public BigDecimal getComItem8() {
            return comItem8;
        }

        /**
         * Sets the value of the comItem8 property.
         * 
         * @param value
         *     allowed object is
         *     {@link BigDecimal }
         *     
         */
        public void setComItem8(BigDecimal value) {
            this.comItem8 = value;
        }

        public boolean isSetComItem8() {
            return (this.comItem8 != null);
        }

    }
    
    /**
     * Inner complex item.
     *
     */
    @XmlAccessorType(XmlAccessType.FIELD)
    @XmlType(name = "ComArray1", propOrder = {
        "comItem2",
        "comGroup1",
        "comItem7"
    })
    public class ComArray1
        implements Serializable
    {

        private final static long serialVersionUID = 1L;
        @CobolElement(cobolName = "COM-ITEM2", type = CobolType.BINARY_ITEM, levelNumber = 10, isSigned = true, totalDigits = 4, picture = "S9(4)", usage = "BINARY", srceLine = 4)
        protected short comItem2;
        @XmlElement(required = true)
        @CobolElement(cobolName = "COM-GROUP1", type = CobolType.GROUP_ITEM, levelNumber = 10, srceLine = 5)
        protected ComGroup1 comGroup1;
        @CobolElement(cobolName = "COM-ITEM7", type = CobolType.BINARY_ITEM, levelNumber = 10, isSigned = true, totalDigits = 8, picture = "S9(8)", usage = "BINARY", srceLine = 12)
        protected int comItem7;

        /**
         * Gets the value of the comItem2 property.
         * 
         */
        public short getComItem2() {
            return comItem2;
        }

        /**
         * Sets the value of the comItem2 property.
         * 
         */
        public void setComItem2(short value) {
            this.comItem2 = value;
        }

        public boolean isSetComItem2() {
            return true;
        }

        /**
         * Gets the value of the comGroup1 property.
         * 
         * @return
         *     possible object is
         *     {@link ComGroup1 }
         *     
         */
        public ComGroup1 getComGroup1() {
            return comGroup1;
        }

        /**
         * Sets the value of the comGroup1 property.
         * 
         * @param value
         *     allowed object is
         *     {@link ComGroup1 }
         *     
         */
        public void setComGroup1(ComGroup1 value) {
            this.comGroup1 = value;
        }

        public boolean isSetComGroup1() {
            return (this.comGroup1 != null);
        }

        /**
         * Gets the value of the comItem7 property.
         * 
         */
        public int getComItem7() {
            return comItem7;
        }

        /**
         * Sets the value of the comItem7 property.
         * 
         */
        public void setComItem7(int value) {
            this.comItem7 = value;
        }

        public boolean isSetComItem7() {
            return true;
        }

    }
    
    /**
     * Inner complex group.
     *
     */
    @XmlAccessorType(XmlAccessType.FIELD)
    @XmlType(name = "ComGroup1", propOrder = {
        "comItem3",
        "comArray2",
        "comItem6"
    })
    public class ComGroup1
        implements Serializable
    {

        private final static long serialVersionUID = 1L;
        @CobolElement(cobolName = "COM-ITEM3", type = CobolType.BINARY_ITEM, levelNumber = 15, isSigned = true, totalDigits = 4, picture = "S9(4)", usage = "BINARY", srceLine = 6)
        protected short comItem3;
        @XmlElement(required = true)
        @CobolElement(cobolName = "COM-ARRAY2", type = CobolType.GROUP_ITEM, levelNumber = 15, minOccurs = 2, maxOccurs = 2, srceLine = 7)
        protected List<ComArray2> comArray2;
        @CobolElement(cobolName = "COM-ITEM6", type = CobolType.BINARY_ITEM, levelNumber = 15, isSigned = true, totalDigits = 4, picture = "S9(4)", usage = "BINARY", srceLine = 11)
        protected short comItem6;

        /**
         * Gets the value of the comItem3 property.
         * 
         */
        public short getComItem3() {
            return comItem3;
        }

        /**
         * Sets the value of the comItem3 property.
         * 
         */
        public void setComItem3(short value) {
            this.comItem3 = value;
        }

        public boolean isSetComItem3() {
            return true;
        }

        /**
         * Gets the value of the comArray2 property.
         * 
         * <p>
         * This accessor method returns a reference to the live list,
         * not a snapshot. Therefore any modification you make to the
         * returned list will be present inside the JAXB object.
         * This is why there is not a <CODE>set</CODE> method for the comArray2 property.
         * 
         * <p>
         * For example, to add a new item, do as follows:
         * <pre>
         *    getComArray2().add(newItem);
         * </pre>
         * 
         * 
         * <p>
         * Objects of the following type(s) are allowed in the list
         * {@link ComArray2 }
         * 
         * 
         */
        public List<ComArray2> getComArray2() {
            if (comArray2 == null) {
                comArray2 = new ArrayList<ComArray2>();
            }
            return this.comArray2;
        }

        public boolean isSetComArray2() {
            return ((this.comArray2 != null)&&(!this.comArray2 .isEmpty()));
        }

        public void unsetComArray2() {
            this.comArray2 = null;
        }

        /**
         * Gets the value of the comItem6 property.
         * 
         */
        public short getComItem6() {
            return comItem6;
        }

        /**
         * Sets the value of the comItem6 property.
         * 
         */
        public void setComItem6(short value) {
            this.comItem6 = value;
        }

        public boolean isSetComItem6() {
            return true;
        }

    }
    
    /**
     * Deep inner complex array.
     *
     */
    @XmlAccessorType(XmlAccessType.FIELD)
    @XmlType(name = "ComArray2", propOrder = {
        "comItem4",
        "comArray3",
        "comItem5"
    })
    public class ComArray2
        implements Serializable
    {

        private final static long serialVersionUID = 1L;
        @XmlElement(required = true)
        @CobolElement(cobolName = "COM-ITEM4", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 20, picture = "X", srceLine = 8)
        protected String comItem4;
        @XmlElement(required = true)
        @CobolElement(cobolName = "COM-ARRAY3", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 20, minOccurs = 5, maxOccurs = 5, picture = "X", srceLine = 9)
        protected List<String> comArray3;
        @XmlElement(required = true)
        @CobolElement(cobolName = "COM-ITEM5", type = CobolType.PACKED_DECIMAL_ITEM, levelNumber = 20, isSigned = false, totalDigits = 7, fractionDigits = 2, picture = "9(5)V99", usage = "PACKED-DECIMAL", srceLine = 10)
        protected BigDecimal comItem5;

        /**
         * Gets the value of the comItem4 property.
         * 
         * @return
         *     possible object is
         *     {@link String }
         *     
         */
        public String getComItem4() {
            return comItem4;
        }

        /**
         * Sets the value of the comItem4 property.
         * 
         * @param value
         *     allowed object is
         *     {@link String }
         *     
         */
        public void setComItem4(String value) {
            this.comItem4 = value;
        }

        public boolean isSetComItem4() {
            return (this.comItem4 != null);
        }

        /**
         * Gets the value of the comArray3 property.
         * 
         * <p>
         * This accessor method returns a reference to the live list,
         * not a snapshot. Therefore any modification you make to the
         * returned list will be present inside the JAXB object.
         * This is why there is not a <CODE>set</CODE> method for the comArray3 property.
         * 
         * <p>
         * For example, to add a new item, do as follows:
         * <pre>
         *    getComArray3().add(newItem);
         * </pre>
         * 
         * 
         * <p>
         * Objects of the following type(s) are allowed in the list
         * {@link String }
         * 
         * 
         */
        public List<String> getComArray3() {
            if (comArray3 == null) {
                comArray3 = new ArrayList<String>();
            }
            return this.comArray3;
        }

        public boolean isSetComArray3() {
            return ((this.comArray3 != null)&&(!this.comArray3 .isEmpty()));
        }

        public void unsetComArray3() {
            this.comArray3 = null;
        }

        /**
         * Gets the value of the comItem5 property.
         * 
         * @return
         *     possible object is
         *     {@link BigDecimal }
         *     
         */
        public BigDecimal getComItem5() {
            return comItem5;
        }

        /**
         * Sets the value of the comItem5 property.
         * 
         * @param value
         *     allowed object is
         *     {@link BigDecimal }
         *     
         */
        public void setComItem5(BigDecimal value) {
            this.comItem5 = value;
        }

        public boolean isSetComItem5() {
            return (this.comItem5 != null);
        }

    }
    
    /**
     * A mock object factory.
     *
     */
    public class Stru04RecordFactory {


        /**
         * Create an instance of {@link ComGroup1 }
         * 
         */
        public ComGroup1 createAbstractVisitorTest$ComGroup1() {
            return new ComGroup1();
        }

        /**
         * Create an instance of {@link ComArray1 }
         * 
         */
        public ComArray1 createAbstractVisitorTest$ComArray1() {
            return new ComArray1();
        }

        /**
         * Create an instance of {@link Stru04Record }
         * 
         */
        public Stru04Record createAbstractVisitorTest$Stru04Record() {
            return new Stru04Record();
        }

        /**
         * Create an instance of {@link ComArray2 }
         * 
         */
        public ComArray2 createAbstractVisitorTest$ComArray2() {
            return new ComArray2();
        }


    }
    
    /**
     * A structure with a redefine.
     *
     */
    @XmlAccessorType(XmlAccessType.FIELD)
    @XmlType(name = "Rdef01Record", propOrder = {
        "comSelect",
        "comDetail1",
        "comDetail2"
    })
    public class Rdef01Record
        implements Serializable
    {

        private final static long serialVersionUID = 1L;
        @CobolElement(cobolName = "COM-SELECT", type = CobolType.BINARY_ITEM, levelNumber = 5, isSigned = false, totalDigits = 4, picture = "9(4)", usage = "BINARY", srceLine = 2)
        protected int comSelect;
        @CobolElement(cobolName = "COM-DETAIL1", type = CobolType.GROUP_ITEM, levelNumber = 5, isRedefined = true, unmarshalChoiceStrategyClassName = "com.legstar.coxb.impl.visitor.AbstractVisitorTest$ComDetailChoice", srceLine = 5)
        protected ComDetail1 comDetail1;
        @CobolElement(cobolName = "COM-DETAIL2", type = CobolType.GROUP_ITEM, levelNumber = 5, redefines = "COM-DETAIL1", srceLine = 7)
        protected ComDetail2 comDetail2;

        /**
         * Gets the value of the comSelect property.
         * 
         */
        public int getComSelect() {
            return comSelect;
        }

        /**
         * Sets the value of the comSelect property.
         * 
         */
        public void setComSelect(int value) {
            this.comSelect = value;
        }

        public boolean isSetComSelect() {
            return true;
        }

        /**
         * Gets the value of the comDetail1 property.
         * 
         * @return
         *     possible object is
         *     {@link ComDetail1 }
         *     
         */
        public ComDetail1 getComDetail1() {
            return comDetail1;
        }

        /**
         * Sets the value of the comDetail1 property.
         * 
         * @param value
         *     allowed object is
         *     {@link ComDetail1 }
         *     
         */
        public void setComDetail1(ComDetail1 value) {
            this.comDetail1 = value;
        }

        public boolean isSetComDetail1() {
            return (this.comDetail1 != null);
        }

        /**
         * Gets the value of the comDetail2 property.
         * 
         * @return
         *     possible object is
         *     {@link ComDetail2 }
         *     
         */
        public ComDetail2 getComDetail2() {
            return comDetail2;
        }

        /**
         * Sets the value of the comDetail2 property.
         * 
         * @param value
         *     allowed object is
         *     {@link ComDetail2 }
         *     
         */
        public void setComDetail2(ComDetail2 value) {
            this.comDetail2 = value;
        }

        public boolean isSetComDetail2() {
            return (this.comDetail2 != null);
        }

    }
    
    /**
     * First alternative.
     *
     */
    @XmlAccessorType(XmlAccessType.FIELD)
    @XmlType(name = "ComDetail1", propOrder = {
        "comName"
    })
    public class ComDetail1
        implements Serializable
    {

        private final static long serialVersionUID = 1L;
        @XmlElement(required = true)
        @CobolElement(cobolName = "COM-NAME", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 10, picture = "X(10)", srceLine = 6)
        protected String comName;

        /**
         * Gets the value of the comName property.
         * 
         * @return
         *     possible object is
         *     {@link String }
         *     
         */
        public String getComName() {
            return comName;
        }

        /**
         * Sets the value of the comName property.
         * 
         * @param value
         *     allowed object is
         *     {@link String }
         *     
         */
        public void setComName(String value) {
            this.comName = value;
        }

        public boolean isSetComName() {
            return (this.comName!= null);
        }

    }

    /**
     * Second alternative.
     *
     */
    @XmlAccessorType(XmlAccessType.FIELD)
    @XmlType(name = "ComDetail2", propOrder = {
        "comAmount"
    })
    public class ComDetail2
        implements Serializable
    {

        private final static long serialVersionUID = 1L;
        @XmlElement(required = true)
        @CobolElement(cobolName = "COM-AMOUNT", type = CobolType.PACKED_DECIMAL_ITEM, levelNumber = 10, isSigned = false, totalDigits = 7, fractionDigits = 2, picture = "9(5)V99", usage = "PACKED-DECIMAL", srceLine = 8)
        protected BigDecimal comAmount;

        /**
         * Gets the value of the comAmount property.
         * 
         * @return
         *     possible object is
         *     {@link BigDecimal }
         *     
         */
        public BigDecimal getComAmount() {
            return comAmount;
        }

        /**
         * Sets the value of the comAmount property.
         * 
         * @param value
         *     allowed object is
         *     {@link BigDecimal }
         *     
         */
        public void setComAmount(BigDecimal value) {
            this.comAmount = value;
        }

        public boolean isSetComAmount() {
            return (this.comAmount!= null);
        }

    }
    
    /**
     * A mock object factory.
     *
     */
    public class Rdef01RecordFactory {

        /**
         * Create an instance of {@link ComDetail1 }
         * 
         */
        public ComDetail1 createAbstractVisitorTest$ComDetail1() {
            return new ComDetail1();
        }

        /**
         * Create an instance of {@link ComDetail2 }
         * 
         */
        public ComDetail2 createAbstractVisitorTest$ComDetail2() {
            return new ComDetail2();
        }

        /**
         * Create an instance of {@link Rdef01Record }
         * 
         */
        public Rdef01Record createAbstractVisitorTest$Rdef01Record() {
            return new Rdef01Record();
        }

    }

    /** 
     * Skeleton implementation of a custom choice selection strategy. Modify this
     * code to select a suitable alternative.
     */
    public static class ComDetailChoice implements ICobolUnmarshalChoiceStrategy {

        public ComDetailChoice() {
            
        }
        
        /** {@inheritDoc} */
        public ICobolBinding choose(
            final ICobolChoiceBinding choice,
            final Hashtable < String, Object > variablesMap,
            final CobolElementVisitor visitor) throws HostException {
            
            /* Get the parent value object which properties might help select the
             * right alternative. */
            Rdef01Record valueObject = (Rdef01Record) choice.getParentValueObject();
            assert (valueObject != null);
            
            /* Replace following code with actual logic. */
            int index = valueObject.getComSelect();
            switch (index) {
            case 0:
                return choice.getAlternativeByName("ComDetail1");
            case 1:
                return choice.getAlternativeByName("ComDetail2");
            case -1:
                /* An exemple of how to signal an exception.*/
                throw (new HostException("Unable to select an alternative"));
            default:
                /* Null will let the default choice strategy apply. */
                return null;
            }
        }
    }
    
    
    /**
     * Case with ambiguous field names.
     *
     */
    @XmlAccessorType(XmlAccessType.FIELD)
    @XmlType(name = "Stru05Record", propOrder = {
        "comItemA",
        "comItemC",
        "comItemD"
    })
    public class Stru05Record
        implements Serializable
    {

        private final static long serialVersionUID = 1L;
        @XmlElement(required = true)
        @CobolElement(cobolName = "COM-ITEM-A", type = CobolType.GROUP_ITEM, levelNumber = 5, srceLine = 2)
        protected ComItemA comItemA;
        @XmlElement(required = true)
        @CobolElement(cobolName = "COM-ITEM-C", type = CobolType.GROUP_ITEM, levelNumber = 5, minOccurs = 3, maxOccurs = 3, srceLine = 4)
        protected List<ComItemC> comItemC;
        @XmlElement(required = true)
        @CobolElement(cobolName = "COM-ITEM-D", type = CobolType.GROUP_ITEM, levelNumber = 5, srceLine = 6)
        protected ComItemD comItemD;

        /**
         * Gets the value of the comItemA property.
         * 
         * @return
         *     possible object is
         *     {@link ComItemA }
         *     
         */
        public ComItemA getComItemA() {
            return comItemA;
        }

        /**
         * Sets the value of the comItemA property.
         * 
         * @param value
         *     allowed object is
         *     {@link ComItemA }
         *     
         */
        public void setComItemA(ComItemA value) {
            this.comItemA = value;
        }

        public boolean isSetComItemA() {
            return (this.comItemA!= null);
        }

        /**
         * Gets the value of the comItemC property.
         * 
         * <p>
         * This accessor method returns a reference to the live list,
         * not a snapshot. Therefore any modification you make to the
         * returned list will be present inside the JAXB object.
         * This is why there is not a <CODE>set</CODE> method for the comItemC property.
         * 
         * <p>
         * For example, to add a new item, do as follows:
         * <pre>
         *    getComItemC().add(newItem);
         * </pre>
         * 
         * 
         * <p>
         * Objects of the following type(s) are allowed in the list
         * {@link ComItemC }
         * 
         * 
         */
        public List<ComItemC> getComItemC() {
            if (comItemC == null) {
                comItemC = new ArrayList<ComItemC>();
            }
            return this.comItemC;
        }

        public boolean isSetComItemC() {
            return ((this.comItemC!= null)&&(!this.comItemC.isEmpty()));
        }

        public void unsetComItemC() {
            this.comItemC = null;
        }

        /**
         * Gets the value of the comItemD property.
         * 
         * @return
         *     possible object is
         *     {@link ComItemD }
         *     
         */
        public ComItemD getComItemD() {
            return comItemD;
        }

        /**
         * Sets the value of the comItemD property.
         * 
         * @param value
         *     allowed object is
         *     {@link ComItemD }
         *     
         */
        public void setComItemD(ComItemD value) {
            this.comItemD = value;
        }

        public boolean isSetComItemD() {
            return (this.comItemD!= null);
        }

    }
    
    
    /**
     * Item A contains Item B.
     *
     */
    @XmlAccessorType(XmlAccessType.FIELD)
    @XmlType(name = "ComItemA", propOrder = {
        "comItemB"
    })
    public class ComItemA
        implements Serializable
    {

        private final static long serialVersionUID = 1L;
        @XmlElement(required = true)
        @CobolElement(cobolName = "COM-ITEM-B", type = CobolType.PACKED_DECIMAL_ITEM, levelNumber = 10, isSigned = false, totalDigits = 7, fractionDigits = 2, picture = "9(5)V99", usage = "PACKED-DECIMAL", srceLine = 3)
        protected BigDecimal comItemB;

        /**
         * Gets the value of the comItemB property.
         * 
         * @return
         *     possible object is
         *     {@link BigDecimal }
         *     
         */
        public BigDecimal getComItemB() {
            return comItemB;
        }

        /**
         * Sets the value of the comItemB property.
         * 
         * @param value
         *     allowed object is
         *     {@link BigDecimal }
         *     
         */
        public void setComItemB(BigDecimal value) {
            this.comItemB = value;
        }

        public boolean isSetComItemB() {
            return (this.comItemB!= null);
        }

    }

    /**
     * Item C contains item B.
     *
     */
    @XmlAccessorType(XmlAccessType.FIELD)
    @XmlType(name = "ComItemC", propOrder = {
        "comItemB"
    })
    public class ComItemC
        implements Serializable
    {

        private final static long serialVersionUID = 1L;
        @CobolElement(cobolName = "COM-ITEM-B", type = CobolType.BINARY_ITEM, levelNumber = 10, isSigned = true, totalDigits = 4, picture = "S9(4)", usage = "BINARY", srceLine = 5)
        protected short comItemB;

        /**
         * Gets the value of the comItemB property.
         * 
         */
        public short getComItemB() {
            return comItemB;
        }

        /**
         * Sets the value of the comItemB property.
         * 
         */
        public void setComItemB(short value) {
            this.comItemB = value;
        }

        public boolean isSetComItemB() {
            return true;
        }

    }
    
    
    /**
     * Item D contains Item E.
     *
     */
    @XmlAccessorType(XmlAccessType.FIELD)
    @XmlType(name = "ComItemD", propOrder = {
        "comItemE"
    })
    public class ComItemD
        implements Serializable
    {

        private final static long serialVersionUID = 1L;
        @XmlElement(required = true)
        @CobolElement(cobolName = "COM-ITEM-E", type = CobolType.GROUP_ITEM, levelNumber = 10, srceLine = 7)
        protected ComItemE comItemE;

        /**
         * Gets the value of the comItemE property.
         * 
         * @return
         *     possible object is
         *     {@link ComItemE }
         *     
         */
        public ComItemE getComItemE() {
            return comItemE;
        }

        /**
         * Sets the value of the comItemE property.
         * 
         * @param value
         *     allowed object is
         *     {@link ComItemE }
         *     
         */
        public void setComItemE(ComItemE value) {
            this.comItemE = value;
        }

        public boolean isSetComItemE() {
            return (this.comItemE!= null);
        }

    }

    /**
     * Item E contains Item B.
     *
     */
    @XmlAccessorType(XmlAccessType.FIELD)
    @XmlType(name = "ComItemE", propOrder = {
        "comItemB"
    })
    public class ComItemE
        implements Serializable
    {

        private final static long serialVersionUID = 1L;
        @XmlElement(required = true)
        @CobolElement(cobolName = "COM-ITEM-B", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 15, picture = "X(4)", srceLine = 8)
        protected String comItemB;

        /**
         * Gets the value of the comItemB property.
         * 
         * @return
         *     possible object is
         *     {@link String }
         *     
         */
        public String getComItemB() {
            return comItemB;
        }

        /**
         * Sets the value of the comItemB property.
         * 
         * @param value
         *     allowed object is
         *     {@link String }
         *     
         */
        public void setComItemB(String value) {
            this.comItemB = value;
        }

        public boolean isSetComItemB() {
            return (this.comItemB!= null);
        }

    }
    public class Stru05RecordFactory {

        /**
         * Create an instance of {@link ComItemE }
         * 
         */
        public ComItemE createAbstractVisitorTest$ComItemE() {
            return new ComItemE();
        }

        /**
         * Create an instance of {@link ComItemD }
         * 
         */
        public ComItemD createAbstractVisitorTest$ComItemD() {
            return new ComItemD();
        }

        /**
         * Create an instance of {@link ComItemC }
         * 
         */
        public ComItemC createAbstractVisitorTest$ComItemC() {
            return new ComItemC();
        }

        /**
         * Create an instance of {@link ComItemA }
         * 
         */
        public ComItemA createAbstractVisitorTest$ComItemA() {
            return new ComItemA();
        }

        /**
         * Create an instance of {@link Stru05Record }
         * 
         */
        public Stru05Record createAbstractVisitorTest$Stru05Record() {
            return new Stru05Record();
        }

    }
}

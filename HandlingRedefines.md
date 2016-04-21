# Introduction #

COBOL REDEFINES are similar to unions in C, multiple structures can share the same memory location.

LegStar provides a default strategy to handle REDEFINES but choosing between multiple alternatives sometimes requires additional logic. Such additional logic, called a ChoiceSelector is java code that you write and gets invoked by LegStar at runtime.

This wiki describes the default strategy and how to add custom logic if needed.

# Mapping COBOL REDEFINES to XSD and Java #

The LegStar [COBOL Structures Mapping Generator (Schemagen)](http://www.legsem.com/legstar/legstar-schemagen/index.html) turns COBOL REDEFINES to XML Schema choices (xsd:choice).

Java does not have a union type though. When binding the XSD to Java using JAXB, the various alternatives all become java objects. Any of these alternative java objects can be null which would indicate that the alternative was not chosen.

This java mapping to xsd:choice is not perfect, for instance multiple java alternatives can be non null at the same time. As it turns out this can help for certain REDEFINES situations. The semantic of REDEFINES is not exactly identical to xsd:choice, it does not mandate that only one alternative is valid at a time.

# Default COBOL REDEFINES binding strategy #

From java to COBOL, the java developer is in control. It is up to him to give a value to the selected alternative and leave the other alternatives as null. The LegStar default strategy tries alternatives one by one, in the order the alternatives were declared in the COBOL structure. The first non null alternative is used and the others are ignored.

From COBOL to java, LegStar must make the decision to map the mainframe data into one of the java alternatives. Again the default strategy tries each alternative in turn (in the COBOL structure order) and starts unmarshaling the mainframe data. If the unmarshaling process fails (usually because the mainframe data does not match the java type), the LegStar default strategy recovers and tries the next alternative until one is unmarshaled successfully.

# Custom COBOL REDEFINES binding strategy #

Sometimes the LegStar default strategy is not what you want. In these cases you need to be able to inject more logic into the marshaling/unmarshaling process to chose the right alternative.

There are 2 attributes to support this customization: unmarshalChoiceStrategyClassName and marshalChoiceStrategyClassName. This is an example of how you would be using unmarshalChoiceStrategyClassName, the most common one:

Using this COBOL structure as an example:
```
       01 DFHCOMMAREA.
           05  C-OUTPUT-TYPE               PIC X(6).
               88  A-NORMAL     VALUE 'normal'.
               88  A-ERROR      VALUE 'error'.
           03  C-DATA                      PIC X(200).
           03  FILLER       REDEFINES C-DATA.
               05  C-STRING                PIC X(30).
               05  FILLER                  PIC X(170).
           03  FILLER       REDEFINES C-DATA.
               05  C-ERROR-NUM             PIC 9(4).
               05  C-ERROR-DESCRIPTION     PIC X(196).
```

When you run legstar-schemagen, you get this XML schema fragment:
```
...
     <xs:choice>
         <xs:element name="CData">
           <xs:annotation>
             <xs:appinfo>
             <cb:cobolElement levelNumber='03' cobolName='C-DATA' type='ALPHANUMERIC_ITEM' picture='X(200)' usage='DISPLAY' isRedefined='true' srceLine='34'/>
             </xs:appinfo>
           </xs:annotation>
           <xs:simpleType>
             <xs:restriction base="xs:string">
             <xs:length value='200'/>
             </xs:restriction>
           </xs:simpleType>
         </xs:element>
         <xs:element name="Filler35" type="xsns:Filler35">
           <xs:annotation>
             <xs:appinfo>
             <cb:cobolElement levelNumber='03' cobolName='FILLER-35' type='GROUP_ITEM' redefines='C-DATA' srceLine='35'/>
             </xs:appinfo>
           </xs:annotation>
         </xs:element>
         <xs:element name="Filler38" type="xsns:Filler38">
           <xs:annotation>
             <xs:appinfo>
             <cb:cobolElement levelNumber='03' cobolName='FILLER-38' type='GROUP_ITEM' redefines='C-DATA' srceLine='38'/>
             </xs:appinfo>
           </xs:annotation>
         </xs:element>
     </xs:choice>
...
```

You must edit the XML schema to inject the unmarshalChoiceStrategyClassName attribute. You do this on the first alternative only (the one that is REDEFINED by the other alternatives):

```
...
     <xs:choice>
         <xs:element name="CData">
           <xs:annotation>
             <xs:appinfo>
             <cb:cobolElement levelNumber='03' cobolName='C-DATA' unmarshalChoiceStrategyClassName='com.legstar.coxb.cust.redmulti.ChoiceSelector' type='ALPHANUMERIC_ITEM' picture='X(200)' usage='DISPLAY' isRedefined='true' srceLine='34'/>
             </xs:appinfo>
           </xs:annotation>
           <xs:simpleType>
             <xs:restriction base="xs:string">
             <xs:length value='200'/>
             </xs:restriction>
           </xs:simpleType>
         </xs:element>
         <xs:element name="Filler35" type="xsns:Filler35">
           <xs:annotation>
             <xs:appinfo>
             <cb:cobolElement levelNumber='03' cobolName='FILLER-35' type='GROUP_ITEM' redefines='C-DATA' srceLine='35'/>
             </xs:appinfo>
           </xs:annotation>
         </xs:element>
         <xs:element name="Filler38" type="xsns:Filler38">
           <xs:annotation>
             <xs:appinfo>
             <cb:cobolElement levelNumber='03' cobolName='FILLER-38' type='GROUP_ITEM' redefines='C-DATA' srceLine='38'/>
             </xs:appinfo>
           </xs:annotation>
         </xs:element>
     </xs:choice>
...
```

Now, when you run the COBOL Binding generator, there is a skeleton ChoiceSelector class that is generated for you. This is where you need to add your custom code. Don't worry if you have to rebind later, LegStar will not overwrite ChoiceSelector if it already exists. The skeleton code looks like this:

```
/** 
 * Skeleton implementation of a custom choice selection strategy. Modify this
 * code to select a suitable alternative.
 */
public class ChoiceSelector implements ICobolUnmarshalChoiceStrategy {

    /** {@inheritDoc} */
    public ICobolBinding choose(
        final ICobolChoiceBinding choice,
        final Hashtable < String, Object > variablesMap,
        final CobolElementVisitor visitor) throws HostException {
        
        /* Get the parent value object which properties might help select the
         * right alternative. */
        Dfhcommarea valueObject = (Dfhcommarea) choice.getObjectValue(Dfhcommarea.class);
        assert (valueObject != null);
        
        /* Replace following code with actual logic. */
        int index = 0;
        switch (index) {
        case 0:
            return choice.getAlternativeByName("CData");
        case 1:
            return choice.getAlternativeByName("Filler35");
        case 2:
            return choice.getAlternativeByName("Filler38");
        case -1:
            /* An exemple of how to signal an exception.*/
            throw (new HostException("Unable to select an alternative"));
        default:
            /* Null will let the default choice strategy apply. */
            return null;
        }
    }
}

```

The are examples of actual [ChoiceSelectors in the LegStar code base](http://code.google.com/p/legstar/source/browse/trunk/#trunk/legstar-coxbrt/src/test/cust/java/com/legstar/coxb/cust) that you might find helpful.







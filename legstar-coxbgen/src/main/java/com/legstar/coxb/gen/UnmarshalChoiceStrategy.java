package com.legstar.coxb.gen;

/**
 * Represents a custom choice strategy class name and the associated COBOL
 * redefined item.
 * <p/>
 * A COBOL redefined item is one which is the object of one or more COBOL
 * REDEFINES clause.
 * 
 */
public class UnmarshalChoiceStrategy {

    /** The redefined COBOL item. */
    private String _redefinedCobolItem;

    /** The unmarshal strategy class name. */
    private String _unmarshalChoiceStrategyClassName;

    /**
     * No-arg constructor.
     */
    public UnmarshalChoiceStrategy() {

    }

    /**
     * Deserialize from string.
     */
    public UnmarshalChoiceStrategy(final String fromString) {
        String[] strings = fromString.split(":");
        if (strings.length > 0) {
            _redefinedCobolItem = strings[0];
        }
        if (strings.length > 1) {
            _unmarshalChoiceStrategyClassName = strings[1];
        }
    }

    /**
     * @param redefinedCobolItem the redefined COBOL item
     * @param unmarshalChoiceStrategyClassName the unmarshal strategy class name
     */
    public UnmarshalChoiceStrategy(final String redefinedCobolItem,
            final String unmarshalChoiceStrategyClassName) {
        _redefinedCobolItem = redefinedCobolItem;
        _unmarshalChoiceStrategyClassName = unmarshalChoiceStrategyClassName;
    }

    /**
     * @return the redefined COBOL item
     */
    public String getRedefinedCobolItem() {
        return _redefinedCobolItem;
    }

    /**
     * @param redefinedCobolItem the redefined COBOL item to set
     */
    public void setRedefinedCobolItem(final String redefinedCobolItem) {
        this._redefinedCobolItem = redefinedCobolItem;
    }

    /**
     * @return the unmarshal strategy class name
     */
    public String getUnmarshalChoiceStrategyClassName() {
        return _unmarshalChoiceStrategyClassName;
    }

    /**
     * @param unmarshalChoiceStrategyClassName the unmarshal strategy class name
     *            to set
     */
    public void setUnmarshalChoiceStrategyClassName(
            final String unmarshalChoiceStrategyClassName) {
        this._unmarshalChoiceStrategyClassName = unmarshalChoiceStrategyClassName;
    }

    public String toString() {
        return String.format("%s:%s", getRedefinedCobolItem(),
                getUnmarshalChoiceStrategyClassName());
    }

}

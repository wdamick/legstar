package com.legstar.coxb.cob2trans;

import java.util.EventObject;

/**
 * An event produced by the generator.
 * 
 */
public class Cob2TransEvent extends EventObject {

    /**
     * The types of events available.
     * 
     */
    public enum EventType {
        /** Step uis starting. */
        START,
        /** Step if finished. */
        STOP
    };

    /** Serial ID. */
    private static final long serialVersionUID = 1L;

    /** A description for the step that ended. */
    private String _stepDescription;

    /** Type of event. */
    private EventType _eventType;

    /** The step number. */
    private int _stepNumber;

    /**
     * A step was performed.
     * 
     * @param source the generator
     * @param stepNumber step number
     * @param stepDescription a description for the step that ended
     * @param eventType the event type
     */
    public Cob2TransEvent(final Object source,
            final int stepNumber,
            final String stepDescription,
            final EventType eventType) {
        super(source);
        _stepNumber = stepNumber;
        _stepDescription = stepDescription;
        _eventType = eventType;
    }

    /**
     * @return the description for the step that ended
     */
    public String getStepDescription() {
        return _stepDescription;
    }

    /**
     * @return the event type
     */
    public EventType getEventType() {
        return _eventType;
    }

    /**
     * @return the step number
     */
    public int getStepNumber() {
        return _stepNumber;
    }

    /** {@inheritDoc} */
    public String toString() {
        if (_eventType == Cob2TransEvent.EventType.START) {
            return "Step " + _stepNumber + " started: " + _stepDescription;
        } else {
            return "Step " + _stepNumber + " ended successfully";
        }

    }

}

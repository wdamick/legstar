package com.legstar.test.coxb.issue154.pojo;

public class EnumContainerClass {
	public enum Day {
        SUNDAY, MONDAY, TUESDAY, WEDNESDAY, 
        THURSDAY, FRIDAY, SATURDAY 
    }
    
    private Day day;
    public Day getDay() {
        return day;
    }
    public void setDay(Day day) {
        this.day = day;
    }

}

/*******************************************************************************
 * Copyright (c) 2015 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
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

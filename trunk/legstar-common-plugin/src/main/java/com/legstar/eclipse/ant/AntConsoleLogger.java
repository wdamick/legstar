/*******************************************************************************
 *  LegStar legacy Web-enablement .
 *  Copyright (C) 2007 LegSem
 *  
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *  
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *   
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 *  02110-1301  USA
 *  
 *******************************************************************************/
package com.legstar.eclipse.ant;

import org.apache.tools.ant.DefaultLogger;
import org.eclipse.ui.console.ConsolePlugin;
import org.eclipse.ui.console.IConsole;
import org.eclipse.ui.console.IConsoleManager;
import org.eclipse.ui.console.MessageConsole;
import org.eclipse.ui.console.MessageConsoleStream;


/**
 * This is code available on the internet. It defines a logger to work
 * with AntRunner in order to direct the output of ant projects to a
 * console. This code must be packaged in its own jar file and has to be
 * loaded by ant. This last constraint means the jar file must be copied
 * to: window-->preferences-->ant-->runtime-->global entries.
 *
 */
public class AntConsoleLogger extends DefaultLogger {

    /** The unique ID for this console instance. */
    private static final String CONSOLE_NAME = "Ant Output";
    /** The instance of a console. */
    private static MessageConsole mConsole;
    /** The stream associated with the console. */
    private static MessageConsoleStream mConsoleStream;


    static {
    	mConsole = findConsole(CONSOLE_NAME);
        mConsoleStream = mConsole.newMessageStream();
    }


    /**
     * Constructor gets the workbench to display the console view.
     */
    public AntConsoleLogger() {
        super();
        ConsolePlugin.getDefault().getConsoleManager().
        	showConsoleView(mConsole);
    }


    /**
     * @see org.apache.tools.ant.DefaultLogger#log(java.lang.String)
     * @param message text to log
     */
    @Override
    protected final void log(final String message) {
        super.log(message);
        mConsoleStream.println(message);
    }


    /**
     * Look for an instance of a console, create a new one if none found.
     * @param name the console ID
     * @return the existing of new instance of the console
     */
    private static MessageConsole findConsole(final String name) {
        ConsolePlugin plugin = ConsolePlugin.getDefault();
        IConsoleManager conMan = plugin.getConsoleManager();
        IConsole[] existing = conMan.getConsoles();
        for (int i = 0; i < existing.length; i++) {
           if (name.equals(existing[i].getName())) {
              return (MessageConsole) existing[i];
           }
        }
        //no console found, so create a new one
        MessageConsole myConsole = new MessageConsole(name, null);
        conMan.addConsoles(new IConsole[]{myConsole});
        return myConsole;
     }
}



/*******************************************************************************
 * LegStar legacy Web-enablement .
 * Copyright (C)  2007 LegSem
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301  USA
 * 
 *     
 *****************************************************************************/
package com.legstar.cixs.http.test;

import com.legstar.cixs.coxb.CIXSProgram;


public class DPLARCHTTest extends CICWAbstractTest {

	private CIXSProgram m_program;

	protected void setUp() throws Exception {
		super.setUp();
        /** Create a program object based on properties file */
		m_program = new CIXSProgram("dplarcht.properties");
	}

	protected void tearDown() throws Exception {
		super.tearDown();
	}

	/**
     * Request 5 programs starting with C
     */
    public void test5ProgramsStartingWithC() {
    	//                   TypeMaxItemsPrefix----------PrefixLen-";
    	String strRequest = "000100000005C340404040404040000000001C";
    	byte[] response = testProgram(m_program, strRequest, "5 Programs starting with C", true);
    	assertEquals("000100000005c340404040404040000000001c000000000005c3c1e4c3c1c6c2c5d7d9d6c7d9c1d44040404040c1e2e2c5d4c2d3c5d9404040000000", toHexString(response).substring(0,120));
    }

	/**
     * Request all programs starting with character C
     */
    public void testAllProgramsStartingWithC() {
    	//                   TypeMaxItemsPrefix----------PrefixLen-";
    	String strRequest = "00015C404040C340404040404040000000001C";
    	byte[] response = testProgram(m_program, strRequest, "All Programs starting with C", true);
    	assertEquals("00015c404040c340404040404040000000001c0000000001f4c3c1e4c3c1c6c2c5d7d9d6c7d9c1d44040404040c1e2e2c5d4c2d3c5d9404040000000", toHexString(response).substring(0,120));
    }

    /**
     * Request all programs
     */
    public void testAllPrograms() {
    	//                   TypeMaxItemsPrefix----------PrefixLen-";
    	String strRequest = "00015C4040404040404040404040000000000C";
    	byte[] response = testProgram(m_program, strRequest, "All Programs", true);
    	assertEquals("00015c4040404040404040404040000000000c0000000001f4c1c1d3d7c8c2c5e3d7d9d6c7d9c1d44040404040d3c5f3f7f040404040404040000000", toHexString(response).substring(0,120));
    }

    /**
     * Request all programs starting with DFH$W
     */
    public void testAllProgramsStartingWithDFH$W() {
    	//                   TypeMaxItemsPrefix----------PrefixLen-";
    	String strRequest = "00015C404040C4C6C85BE6404040000000005C";
    	byte[] response = testProgram(m_program, strRequest, "All Programs starting with DFH$W", true);
    	assertEquals("00015c404040c4c6c85be6404040000000005c000000000001c4c6c85be6c2f1c1d7d9d6c7d9c1d44040404040c1e2e2c5d4c2d3c5d9404040000000", toHexString(response).substring(0,120));
    }

    /**
     * Request all programs starting with GABUZOME
     */
    public void testAllProgramsStartingWithGABUZOME() {
    	//                   TypeMaxItemsPrefix----------PrefixLen-";
    	String strRequest = "00015C404040C7C1C2E4E9D6D4C5000000008C";
    	byte[] response = testProgram(m_program, strRequest, "All Programs starting with GABUZOME", true);
    	assertEquals("00015c404040c7c1c2e4e9d6d4c5000000008c0001000000000000000000000000000000000000000000000000000000000000000000000000000000", toHexString(response).substring(0,120));
    }

    /**
     * Request all files
     */
    public void testAllFiles() {
    	//                   TypeMaxItemsPrefix----------PrefixLen-";
    	String strRequest = "00005C4040404040404040404040000000000C";
    	byte[] response = testProgram(m_program, strRequest, "All files", true);
    	assertEquals("00005c4040404040404040404040000000000c000000000004c4c6c8c3e2c44040c3c9c3e2e3e2f2f34bc3c9c3e24bc4c6c8c3e2c440404040404040", toHexString(response).substring(0,120));
    }

    /**
     * Request all files
     */
    public void testAllTransactionsStartingWithA() {
    	//                   TypeMaxItemsPrefix----------PrefixLen-";
    	String strRequest = "00025C404040C140404040404040000000001C";
    	byte[] response = testProgram(m_program, strRequest, "All transactions starting with A", true);
    	assertEquals("00025c404040c140404040404040000000001c000000000008c1c1c4c440404040c4c6c85bc1c1d3d3c5d5c1c2d3c5c4404040404000000000000000", toHexString(response).substring(0,120));
    }

    /**
     * Request more than 500 items
     */
    public void testMoreThan500ItemsRequested() {
    	//                   TypeMaxItemsPrefix----------PrefixLen-";
    	String strRequest = "0001F0F5F0F5C140404040404040000000001C";
    	byte[] response = testProgram(m_program, strRequest, "More than 500 items", true);
    	assertEquals("0001f0f5f0f5c140404040404040000000001c0002d481a78994a4944089a38594a2408381959596a34085a78385858440f0f0f0f0f0f0f5f0f04040", toHexString(response).substring(0,120));
    }

    /**
     * Request search prefix larger than 8
     */
    public void testSearchPrefixLargerThan8() {
    	//                   TypeMaxItemsPrefix----------PrefixLen-";
    	String strRequest = "00015C404040C140404040404040000000009C";
    	byte[] response = testProgram(m_program, strRequest, "Search prefix larger than 8", true);
    	assertEquals("00015c404040c140404040404040000000009c0002a28581998388409799858689a7408381959596a34085a78385858440f840404040404040404040", toHexString(response).substring(0,120));
    }
}

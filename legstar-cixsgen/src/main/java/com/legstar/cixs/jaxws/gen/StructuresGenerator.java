/*******************************************************************************
 * Copyright (c) 2008 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.cixs.jaxws.gen;

import java.util.List;

import com.legstar.cixs.gen.model.CixsOperation;
import com.legstar.cixs.gen.model.CixsStructure;
import com.legstar.cobc.gen.CobolGenSentence;
import com.legstar.cobc.gen.CobolGenerationException;
import com.legstar.cobc.gen.CobolGenerator;

/**
 * This class is a helper used by velocity templates which needs to insert
 * generated COBOL data structures in a COBOL program.
 *
 */
public class StructuresGenerator {

	/** Used for structures to determine if they are input or output. */
    private enum Direction { INPUT, OUTPUT };

	/**
	 * Generates COBOL code for input data structures.
	 * @param operation an operation
	 * @param firstCobolLevel where to start numbering Cobol data items
	 * @param cobolLevelIncrement how much to increment Cobol data items moving
	 * @return the COBOL source code for input data items.
	 * @throws CobolGenerationException if code generation fails
	 */
	public String getInputStructuresCode(
			final CixsOperation operation,
			final int firstCobolLevel,
			final int cobolLevelIncrement) throws CobolGenerationException {
		return getStructuresCode(operation, Direction.INPUT,
				firstCobolLevel, cobolLevelIncrement);
	}

	/**
	 * Generates COBOL code for output data structures.
	 * @param operation an operation
	 * @param firstCobolLevel where to start numbering Cobol data items
	 * @param cobolLevelIncrement how much to increment Cobol data items moving
	 * @return the COBOL source code for output data items.
	 * @throws CobolGenerationException if code generation fails
	 */
	public String getOutputStructuresCode(
			final CixsOperation operation,
			final int firstCobolLevel,
			final int cobolLevelIncrement) throws CobolGenerationException {
		return getStructuresCode(operation, Direction.OUTPUT,
				firstCobolLevel, cobolLevelIncrement);
	}

	/**
	 * Generates COBOL code for data structures. The result is a a
	 * concatenation of the code for each of the inner structures.
	 * @param operation an operation
	 * @param direction either input or output
	 * @param firstCobolLevel where to start numbering Cobol data items
	 * @param cobolLevelIncrement how much to increment Cobol data items moving
	 * @return the COBOL source code for data items
	 * @throws CobolGenerationException if code generation fails
	 */
	private String getStructuresCode(
			final CixsOperation operation,
			final Direction direction,
			final int firstCobolLevel,
			final int cobolLevelIncrement) throws CobolGenerationException {
		StringBuilder sb = new StringBuilder();
		List < CixsStructure > structures = null;

		if (direction == Direction.INPUT) {
			structures = operation.getInput();
		} else {
			structures = operation.getOutput();
		}
		for (CixsStructure structure : structures) {
			sb.append(getStructureCode(structure,
					firstCobolLevel, cobolLevelIncrement));
		}
		return sb.toString();
	}

	/**
	 * Using the <code>cobcgen</code> utility, this will use reflection
	 * to instantiate a Jaxb object corresponding to the structure
	 * received and then generate COBOL data description code using
	 * the COBOL annotations in the jaxb class. 
	 * @param structure the structure for which code is to be generated
	 * @param firstCobolLevel where to start numbering Cobol data items
	 * @param cobolLevelIncrement how much to increment Cobol data items moving
	 * @return data description COBOL source code for the structure
	 * @throws CobolGenerationException if code generation fails
	 */
	public String getStructureCode(
			final CixsStructure structure,
			final int firstCobolLevel,
			final int cobolLevelIncrement) throws CobolGenerationException {
		return CobolGenerator.generate(
				structure.getJaxbPackageName(),
				structure.getJaxbType(),
				structure.getCobolRootDataItemName(),
				firstCobolLevel,
				cobolLevelIncrement);
		
	}
	
	/**
	 * This method formats a value starting at the requested position.
	 * COBOL values may span multiple lines in which case there is a 
	 * need for special continuation rules to apply. This method will deal
	 * with that.
	 * @param value the COBOL value, including start and end delimiters
	 *  (QUOTE or APOST)
	 * @param indentFactor determines if this sentence is to be indented
	 *  relative to area A. An indentation factor of zero means this 
	 *  sentence starts at column 8, an indentation factor of 1 means it starts
	 *  4 characters past column 8 which is column 12 or area B. When the
	 *  indentation factor increases by 1 the starting position of this 
	 *  sentence increases by 4 columns.
	 * @return a formatted code fragment
	 */
	public String getCobolValueCode(
			final String value, final int indentFactor) {
		CobolGenSentence s = new CobolGenSentence(indentFactor);
		s.addValue(value);
		s.close();
		return s.toString();
		
	}


}

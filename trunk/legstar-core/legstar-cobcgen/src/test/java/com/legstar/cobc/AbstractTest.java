package com.legstar.cobc;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.Method;

import junit.framework.TestCase;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public abstract class AbstractTest extends TestCase {

	/** Reference folder. */
	public static final File REF_DIR = new File("src/test/resources/reference");

	/** Produce copybooks in this location. */
	public static final File GEN_DIR = new File("target/gen/cobol");

	/** Extension added to reference files. */
	public static final String REF_FILE_EXT = "txt";

	private static Log logger = LogFactory.getLog(AbstractTest.class);

	public void setUp() throws Exception {
		if (isCreateReferences()) {
			cleanOldReferences();
		}
		GEN_DIR.mkdirs();
	}

	/**
	 * This is our chance to remove reference files that are no longer used by a
	 * test case. This happens when test cases are renamed or removed.
	 */
	protected void cleanOldReferences() {
		if (!getReferenceFolder().exists()) {
			return;
		}
		Method[] methods = getClass().getDeclaredMethods();

		for (File refFile : FileUtils.listFiles(getReferenceFolder(),
				new String[] { REF_FILE_EXT }, false)) {
			boolean found = false;
			for (int i = 0; i < methods.length; i++) {
				if (methods[i].getName().equals(
						FilenameUtils.getBaseName(refFile.getName()))) {
					found = true;
					break;
				}
			}
			if (!found) {
				refFile.delete();
			}
		}
	}

	/**
	 * Check a result against a reference.
	 * 
	 * @throws IOException
	 *             if something fails
	 */
	protected void check(final String result) {
		try {
			logger.debug(getClass().getSimpleName() + "-" + getName() + ":\n"
					+ result);
			File referenceFile = new File(getReferenceFolder(), getName() + "."
					+ REF_FILE_EXT);

			if (isCreateReferences()) {
				FileUtils.writeStringToFile(referenceFile, result, "UTF-8");
			} else {
				String expected = FileUtils.readFileToString(referenceFile,
						"UTF-8");
				// neutralize platform specific line separator
				assertEquals(expected.replaceAll("[\\r\\n]", ""), result
						.replaceAll("[\\r\\n]", ""));
			}
		} catch (IOException e) {
			logger.error("Test " + getName() + " failed", e);
			fail(e.getMessage());
		}

	}

	/**
	 * Location where where reference files are stored for this test case.
	 * 
	 * @return the test case reference files folder
	 */
	public File getReferenceFolder() {
		return new File(REF_DIR, getClass().getSimpleName());
	}

	/**
	 * @return true if references should be created instead of compared to
	 *         results
	 */
	public abstract boolean isCreateReferences();

}

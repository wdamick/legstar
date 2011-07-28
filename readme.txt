Overview:
=========
  The cob2trans utility translates a set of COBOL structure definitions to
  Transformers.
  
  Transformers are java classes that you can use to convert mainframe
  payloads to java objects, XML or JSON.
  
  The utility is available as an executable jar.

Prerequisites:
=============

  You need Java JDK 1.5 or higher.
  
  Make sure you have a JDK since the utility needs access to a compiler.
  
Running the sample using the executable jar:
============================================

  1. Go to the folder where you unzipped the distribution file. It should
     contain run.sh (Linux) and run.bat (Windows) command files.
     The cobol sub folder is used as input. It contains a single COBOL file.
  2. On Linux you probably need to type chmod +x run.sh to make it executable
  3. Type ./run.sh (Linux) or run (Windows).
  4. Check the target sub folder, you should find various artifacts and a
     deployable jar archive in the dist sub folder.

Troubleshooting the executable jar:
===================================

   The conf sub folder contains a log4j configuration file. Set the debug
   level to get more information on errors.

   Checkout the group discussion list at:
   http://groups.google.com/group/legstar-user.
   
   If you can't find a solution, please file a bug report at:
   http://code.google.com/p/legstar/issues/list
   
Additional information:
=======================
  
  Check the wiki pages: http://code.google.com/p/legstar/w/list.
  
  Javadoc: http://www.legsem.com/legstar/legstar-cob2trans/apidocs/index.html 
  
  Join the discussion group: http://groups.google.com/group/legstar-user.

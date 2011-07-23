Overview:
=========
  The purpose of this transport quickstart sample is to test connectivity
  to mainframe using this particular transport.

Prerequisites:
=============
  This module requires a JDK, or an international version of the JRE,
  that includes charsets.jar.

  The LegStar z/OS modules must be installed in the target CICS region.

  Refer to: http://www.legsem.com/legstar/legstar-transport/legstar-distribution-zos/index.html
  for instructions.
  
Settings:
=========
  Check parameters in legstar-invoker-config.xml, they must match your
  mainframe settings.
  

Running this quickstart:
========================

  1. In a command terminal window in this folder ("Window1"), type 'ant runtest'.
  2. Check the messages produced.

Final deployment:
=================
  
  Copy legstar-invoker-config.xml to $CATALINA_HOME/shared/classes

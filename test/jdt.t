  $ source "$TESTDIR/helper.sh"
  If you don't see this, setup failed

jdt

  $ run_restyler jdt ./src/main/java/ShouldFormat1.java ./ShouldFormat2.java
  Calling [mvn, net.revelc.code.formatter:formatter-maven-plugin:2.10.0:format, -Dproject.build.sourceEncoding=UTF-8, -DsourceDirectory=./, -Dformatter.includes=./src/main/java/ShouldFormat1.java,./ShouldFormat2.java]
  [INFO] Scanning for projects...
  [INFO] 
  [INFO] ------------------------< fooGroup:fooArtifact >------------------------
  [INFO] Building fooArtifact 0.0.0-SNAPSHOT
  [INFO] --------------------------------[ jar ]---------------------------------
  [INFO] 
  [INFO] --- formatter-maven-plugin:2.10.0:format (default-cli) @ fooArtifact ---
  [INFO] Using 'UTF-8' encoding to format source files.
  [INFO] Number of files to be formatted: 0
  [INFO] ------------------------------------------------------------------------
  [INFO] BUILD SUCCESS
  [INFO] ------------------------------------------------------------------------
  [INFO] Total time:  * (glob)
  [INFO] Finished at: * (glob)
  [INFO] ------------------------------------------------------------------------
  [INFO] Scanning for projects...
  [INFO] 
  [INFO] ------------------------< fooGroup:fooArtifact >------------------------
  [INFO] Building fooArtifact 0.0.0-SNAPSHOT
  [INFO] --------------------------------[ jar ]---------------------------------
  [INFO] 
  [INFO] --- maven-clean-plugin:2.5:clean (default-clean) @ fooArtifact ---
  [INFO] Deleting /code/target
  [INFO] ------------------------------------------------------------------------
  [INFO] BUILD SUCCESS
  [INFO] ------------------------------------------------------------------------
  [INFO] Total time:  * (glob)
  [INFO] Finished at: * (glob)
  [INFO] ------------------------------------------------------------------------

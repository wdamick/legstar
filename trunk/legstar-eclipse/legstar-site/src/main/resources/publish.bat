java -jar "%ECLIPSE_HOME%/plugins/org.eclipse.equinox.launcher_1.1.0.v20100507.jar" ^
-application org.eclipse.equinox.p2.publisher.UpdateSitePublisher ^
-metadataRepository file:/${legstarPluginVersion} ^
-artifactRepository file:/${legstarPluginVersion} ^
-source /${legstarPluginVersion} ^
-compress  ^
-configs gtk.linux.x86
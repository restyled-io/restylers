import java.io.File;

import java.util.ArrayList;
import java.util.Set;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public class Formatter {

	private static final Set<String> PREFIXES = Set.of(
		"./",
		"src/main/java/",
		"src/test/java/",
		"src/main/resources/",
		"src/test/resources/",
	);

	public static void main(String... args) throws Exception {
		ArrayList<String> files = new ArrayList<>(args.length);
		ArrayList<String> formatterArgs = new ArrayList<>(args.length);
		for(String arg : args) {
			if(new File(arg).exists()) {
				files.add(removePrefix(arg));
			}
			else {
				formatterArgs.add(arg);
			}
		}		
		
		List<String> call = new ArrayList<>();
		call.addAll(Arrays.asList(
			"mvn",
			"net.revelc.code.formatter:formatter-maven-plugin:2.10.0:format",
			"-Dproject.build.sourceEncoding=UTF-8"
		));
		call.addAll(formatterArgs);
		call.add("-Dformatter.includes="+files.stream().collect(Collectors.joining(",")));
		
		System.out.println("Calling "+call);
		int exit = new ProcessBuilder(call)
			.inheritIO()
			.start()
			.waitFor();
		
		new ProcessBuilder("mvn","clean").inheritIO().start().waitFor();
		
		System.exit(exit);
	}
	
	private static String removePrefix(String file) {
		for(String prefix : PREFIXES) {
			if(file.startsWith(prefix)) {
				file = file.substring(prefix.length());
			}
		}
		return file;
	}
}

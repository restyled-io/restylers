import java.io.File;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public class Formatter {

	public static void main(String... args) throws Exception {
		ArrayList<String> files = new ArrayList<>(args.length);
		ArrayList<String> formatterArgs = new ArrayList<>(args.length);
		for(String arg : args) {
			if(new File(arg).exists()) {
				if(arg.startsWith("./")) {
					arg = arg.substring(2);
				}
				files.add(arg);
			}
			else {
				formatterArgs.add(arg);
			}
		}		
		
		List<String> call = new ArrayList<>();
		call.addAll(Arrays.asList(
			"mvn",
			"-o",
			"net.revelc.code.formatter:formatter-maven-plugin:2.10.0:format",
			"-Dproject.build.sourceEncoding=UTF-8"
		));
		call.addAll(formatterArgs);
		call.add("-DsourceDirectory=./");
		call.add("-Dformatter.includes="+files.stream().collect(Collectors.joining(",")));
		
		System.out.println("Calling "+call);
		int exit = new ProcessBuilder(call).inheritIO().start().waitFor();
		
		new ProcessBuilder("mvn","-o","clean").inheritIO().start().waitFor();
		
		System.exit(exit);
	}
}

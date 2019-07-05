import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public class Formatter {
	public static void main(String... args) throws Exception {
		args[0] = Arrays
			.stream(args[0].split(","))
			.map(Formatter::removePrefix)
			.collect(Collectors.joining(","));
		
		
		List<String> call = new ArrayList<>();
		call.addAll(Arrays.asList(
			"mvn",
			"net.revelc.code.formatter:formatter-maven-plugin:2.10.0:format",
			"-Dproject.build.sourceEncoding=UTF-8"
		));
		call.add("-Dformatter.includes="+args[0]);
		call.addAll(Arrays.asList(args).subList(1, args.length));
		
		System.out.println("Calling "+call);
		int exit = new ProcessBuilder(call)
			.inheritIO()
			.start()
			.waitFor();
		
		new ProcessBuilder("mvn","clean").inheritIO().start().waitFor();
		
		System.exit(exit);
	}
	
	private static String removePrefix(String file) {
		if(file.startsWith("./src/main/java/")) {
			return file.substring(16);
		}
		return file;
	}
}

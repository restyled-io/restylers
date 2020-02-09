require "tmpdir"
require "yaml"

class RestylerTests
  DOCKER_RUN = %w[docker run --interactive --rm --net none].freeze

  def initialize(name)
    @name = name
    @info = YAML.safe_load(`build/restyler-meta get #{name} .`)
    @tests = @info.fetch("metadata").fetch("tests")
    @tempdir = Dir.mktmpdir
    @oldpwd = Dir.pwd
  end

  def setup
    Dir.chdir(tempdir)

    tests.each.with_index do |test, i|
      if support = test["support"]
        support_path = support.fetch("path")
        support_contents = support.fetch("contents")

        if File.exist?(support_path) && File.read(support_path) != support_contents
          # TODO: figure out a Real Fix here
          raise "#{name}:#{i} test would clobber #{support_path}. Refusing."
        end

        File.write(support_path, support_contents)
      end

      File.write(testfile_path(test, i), test.fetch("contents"))
    end
  end

  def act
    paths = tests.map.with_index { |test, i| testfile_path(test, i) }

    run_restyler(name, paths)
  end

  def assertions
    tests.each.with_index do |test, i|
      actual = File.read(testfile_path(test, i))
      expected = tests[i].fetch("restyled")
      yield(actual, expected)
    end
  end

  def teardown
    Dir.chdir(oldpwd)
    FileUtils.rm_rf(tempdir)
  end

  private

  attr_reader :name, :info, :tests, :tempdir, :oldpwd

  def testfile_path(test, i)
    ext = test["extension"] || "temp"
    "#{name}-test-#{i}.#{ext}"
  end

  def run_restyler(name, paths)
    restyler_command = DOCKER_RUN.dup
    restyler_command += %W[--volume #{Dir.pwd}:/code]
    restyler_command << info.fetch("image")
    restyler_command += info.fetch("command")
    restyler_command += info.fetch("arguments")
    restyler_command.uniq! # Restyler does this
    restyler_command << "--" if info.fetch("supports_arg_sep")

    paths = paths.map { |p| "./#{p}" } # Restyler does this

    if info.fetch("supports_multiple_paths")
      cmd = restyler_command + paths
      system(*cmd) or return false
    else
      paths.each do |path|
        cmd = restyler_command + [path]
        system(*cmd) or return false
      end
    end

    return true
  end
end

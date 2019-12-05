names = `build/restyler-meta names`.split("\n")
names.each do |name|
  describe "The #{name} Restyler", "#{name}": true do
    before do
      @tests = RestylerTests.new(name)
      @tests.setup
    end

    after { @tests.teardown }

    it "Restyles as expected" do
      expect(@tests.act).to eq(true), "Restyler should exit successfully"
      @tests.assertions { |actual, expected| expect(actual).to eq(expected) }
    end
  end
end

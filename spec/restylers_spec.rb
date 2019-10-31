names = `build/restyler-meta names`.split("\n")
names.each do |name|
  describe "The #{name} Restyler", "#{name}": true do
    before do
      @tests = RestylerTests.new(name)
      @tests.setup
    end

    after do
      @tests.teardown
    end

    it "Restyles as expected" do
      @tests.act
      @tests.assertions do |actual, expected|
        expect(actual).to eq(expected)
      end
    end
  end
end

RSpec.describe Calculator do
  subject { Calculator.new }
  let(:initial_value) { 0 }

  describe "#add" do
    let(:calculator) { Calculator.new }
    let(:amount) { 5 }

    context "with negative numbers" do
      let(:amount) { -5 }

      it "handles negatives" do
        expect(subject.add(amount, 1)).to eq(-4)
      end
    end
  end

  describe "#multiply" do
    let(:factor) { 2 }

    it "multiplies" do
      expect(subject.multiply(3, factor)).to eq(6)
    end
  end

  # Edge cases for receiver filtering:
  Foo.context "invalid" do    # explicit receiver - invalid
  end

  Foo.let(:invalid) { }       # explicit receiver - invalid
end

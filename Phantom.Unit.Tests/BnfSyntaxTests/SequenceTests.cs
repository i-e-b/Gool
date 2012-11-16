using NUnit.Framework;
using Phantom.Parsers.Composite;
using Phantom.Scanners;

namespace Phantom.Unit.Tests.BnfSyntaxTests
{
	[TestFixture]
	public class SequenceTests
	{
		[Test]
		public void bnf_sequence_mark_should_result_in_sequence_parser ()
		{
			var subject = (BNF)"hello" > "world";
			var result = subject.Result();

			Assert.That(result, Is.InstanceOf<Sequence>());
		}

		[Test]
		public void BNF_sequence_matches_input_correctly ()
		{
			var input = new ScanStrings("hello, world"){SkipWhitespace = true};
			var subject = (BNF)"hello" > ',' > "world";
			var result = subject.Result().Parse(input);

			Assert.That(result.Success, Is.True);
			Assert.That(result.Value, Is.EqualTo("hello, world"));
		}
	}
}

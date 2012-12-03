using NUnit.Framework;
using Phantom.Parsers.Composite;
using Phantom.Scanners;

namespace Phantom.Unit.Tests.BnfSyntaxTests
{
	[TestFixture]
	public class ZeroOrOneRepeatTests
	{
		[Test]
		public void bnf_exclaimation_mark_should_result_in_zero_or_one_repetition_parser ()
		{
			var subject = !((BNF)"hello");
			var result = subject.Result();

			Assert.That(result, Is.InstanceOf<Repetition>());
		}

		[Test]
		public void parser_minimum_is_zero ()
		{
			var subject = !((BNF)"hello");
			var result = (Repetition)subject.Result();

			Assert.That(result.LowerBound, Is.EqualTo(0));
		}
		
		[Test]
		public void parser_maximum_is_one ()
		{
			var subject = !((BNF)"hello");
			var result = (Repetition)subject.Result();

			Assert.That(result.UpperBound, Is.GreaterThanOrEqualTo(1));
		}

		[Test]
		public void BNF_terminated_list_matches_input_correctly ()
		{
			var input = new ScanStrings("hello hello jello hello"){SkipWhitespace = true};
			var subject = !((BNF)"hello");
			var result = subject.Result().Parse(input);

			Assert.That(result.Success, Is.True);
			Assert.That(result.Value, Is.EqualTo("hello"));
		}
	}
}

using NUnit.Framework;
using Phantom.Parsers.Composite;
using Phantom.Scanners;

namespace Phantom.Unit.Tests.BnfSyntaxTests
{
	[TestFixture]
	public class OneOrMoreRepeatTests
	{
		[Test]
		public void bnf_plus_sign_should_result_in_one_or_more_repetition_parser ()
		{
			var subject = +((BNF)"hello");
			var result = subject.Result();

			Assert.That(result, Is.InstanceOf<Repetition>());
		}

		[Test]
		public void parser_minimum_is_zero ()
		{
			var subject = +((BNF)"hello");
			var result = (Repetition)subject.Result();

			Assert.That(result.LowerBound, Is.EqualTo(1));
		}
		
		[Test]
		public void parser_maximum_is_very_large ()
		{
			var subject = +((BNF)"hello");
			var result = (Repetition)subject.Result();

			Assert.That(result.UpperBound, Is.GreaterThanOrEqualTo(int.MaxValue));
		}

		[Test]
		public void BNF_terminated_list_matches_input_correctly ()
		{
			var input = new ScanStrings("hello hello jello hello"){SkipWhitespace = true};
			var subject = +((BNF)"hello");
			var result = subject.Result().Parse(input);

			Assert.That(result.Success, Is.True);
			Assert.That(result.Value, Is.EqualTo("hello hello"));
		}
	}
}

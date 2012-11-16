using NUnit.Framework;
using Phantom.Parsers.Composite;
using Phantom.Scanners;

namespace Phantom.Unit.Tests.BnfSyntaxTests
{
	[TestFixture]
	public class DelimitedListTests
	{
		[Test]
		public void bnf_delimited_mark_should_result_in_delimited_list_parser ()
		{
			var subject = (BNF)"hello" % ",";
			var result = subject.Result();

			Assert.That(result, Is.InstanceOf<DelimitedList>());
		}

		[Test]
		public void BNF_delimited_list_matches_input_correctly ()
		{
			var input = new ScanStrings("hello, hello , hello,hello"){SkipWhitespace = true};
			var subject = (BNF)"hello" % ",";
			var result = subject.Result().Parse(input);

			Assert.That(result.Success, Is.True);
			Assert.That(result.Value, Is.EqualTo("hello, hello , hello,hello"));
		}
	}
}

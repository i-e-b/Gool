using NUnit.Framework;
using Phantom.Parsers.Composite;
using Phantom.Parsers.Terminals;
using Phantom.Scanners;

namespace Phantom.Unit.Tests.BnfSyntaxTests
{
	[TestFixture]
	public class Options
	{
		[Test]
		public void bnf_options_result_in_a_union_parser ()
		{
			var subject = (BNF)"one" | "two";
			var result = subject.Result();
			
			Assert.That(result, Is.InstanceOf<Union>());
			Assert.That(((Union)result).LeftParser, Is.InstanceOf<LiteralString>());
			Assert.That(((Union)result).RightParser, Is.InstanceOf<LiteralString>());
		}

		[Test]
		[TestCase("hello world", true)]
		[TestCase("once in a lifetime", true)]
		[TestCase("ancient greece", false)]
		public void bnf_options_concatenate_into_large_options (string source, bool passes)
		{
			var scanner = new ScanStrings(source){SkipWhitespace = true};
			var subject = (BNF)"hello world" | "mind the gap" | "once in a lifetime" | "modern greece";

			var result = subject.Result().Parse(scanner);

			Assert.That(result.Success, Is.EqualTo(passes));
		}
	}
}

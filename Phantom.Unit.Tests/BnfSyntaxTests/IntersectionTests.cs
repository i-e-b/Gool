using NUnit.Framework;
using Phantom.Parsers.Composite;
using Phantom.Parsers.Terminals;
using Phantom.Scanners;

namespace Phantom.Unit.Tests.BnfSyntaxTests
{
	[TestFixture]
	public class IntersectionTests
	{
		[Test]
		public void bnf_ampersand_result_in_an_intersection_parser ()
		{
			var subject = (BNF)"one" & "two";
			var result = subject.Result();
			
			Assert.That(result, Is.InstanceOf<Intersection>());
			Assert.That(((Intersection)result).LeftParser, Is.InstanceOf<LiteralString>());
			Assert.That(((Intersection)result).RightParser, Is.InstanceOf<LiteralString>());
		}

		[Test]
		[TestCase("hello world", true)]
		[TestCase("world hello", true)]
		[TestCase("ancient greece", false)]
		public void bnf_intersections_match_input_correctly (string source, bool passes)
		{
			var scanner = new ScanStrings(source){SkipWhitespace = true};
			var subject = (BNF)"world" & "hello";

			var result = subject.Result().Parse(scanner);

			Assert.That(result.Success, Is.EqualTo(passes), string.Join(", ",scanner.ListFailures()));
		}
	}
}

using NUnit.Framework;
using Phantom.Parsers.Composite;
using Phantom.Parsers.Terminals;
using Phantom.Scanners;

namespace Phantom.Unit.Tests.BnfSyntaxTests
{
	[TestFixture]
	public class ExclusiveOrTests
	{
		[Test]
		public void bnf_circumflex_result_in_an_xor_parser ()
		{
			var subject = (BNF)"one" ^ "two";
			var result = subject.Result();
			
			Assert.That(result, Is.InstanceOf<Exclusive>());
			Assert.That(((Exclusive)result).LeftParser, Is.InstanceOf<LiteralString>());
			Assert.That(((Exclusive)result).RightParser, Is.InstanceOf<LiteralString>());
		}

		[Test]
		[TestCase("hello world", true)]
		[TestCase("hello Princetown", false)]
		[TestCase("ancient greece", false)]
		public void bnf_xors_match_input_correctly (string source, bool passes)
		{
			var scanner = new ScanStrings(source){SkipWhitespace = true};
			var subject = (BNF)"hello" ^ "hello Princetown";

			var result = subject.Result().Parse(scanner);

			Assert.That(result.Success, Is.EqualTo(passes), string.Join(", ",scanner.ListFailures()));
		}
	}
}

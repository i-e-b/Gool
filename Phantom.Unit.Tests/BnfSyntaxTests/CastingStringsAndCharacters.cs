using NUnit.Framework;
using Phantom.Parsers.Terminals;
using Phantom.Scanners;

namespace Phantom.Unit.Tests.BnfSyntaxTests
{
	[TestFixture]
	public class CastingStringsAndCharacters
	{
		[Test]
		public void casting_a_string_to_BNF_results_in_a_literal_string ()
		{
			var subject = (BNF)"hello world";
			var result = subject.Result();

			Assert.That(result, Is.InstanceOf<LiteralString>());
		}

		[Test]
		public void casting_a_character_to_BNF_results_in_a_literal_character ()
		{
			var subject = (BNF)';';
			var result = subject.Result();

			Assert.That(result, Is.InstanceOf<LiteralCharacter>());
		}

		[Test]
		public void casting_a_string_starting_with_hash_results_in_a_RegularExpression ()
		{
			var subject = (BNF)"#abc";
			var result = subject.Result();

			Assert.That(result, Is.InstanceOf<RegularExpression>());
			Assert.That(result.Parse(new ScanStrings("abc")).Success, Is.True);
		}

		[Test]
		public void casting_a_string_starting_with_two_hash_marks_results_in_a_literal_string_for_one_hash_mark ()
		{
			var subject = (BNF)"##abc";
			var result = subject.Result();

			Assert.That(result, Is.InstanceOf<LiteralString>());
			Assert.That(result.Parse(new ScanStrings("#abc")).Success, Is.True);
		}
	}
}

using NUnit.Framework;
using Phantom.Parsers;
using Phantom.Parsers.Terminals;

namespace Phantom.Unit.Tests.BnfSyntaxTests
{
	[TestFixture]
	public class CastingParsers
	{
		[Test]
		public void casting_a_parser_to_a_BNF_wraps_that_parser ()
		{
			Parser parser = new LiteralCharacter('?');
			BNF subject = parser; //must be class, can't cast interfaces :-(

			var result = subject.Result();

			Assert.That(result, Is.InstanceOf<LiteralCharacter>());
		}

		[Test]
		public void explicitly_casting_a_parser_to_a_BNF_wraps_that_parser ()
		{
			Parser parser = new LiteralCharacter(';');
			var subject = (BNF)parser;

			var result = subject.Result();

			Assert.That(result, Is.InstanceOf<LiteralCharacter>());
		}
	}
}

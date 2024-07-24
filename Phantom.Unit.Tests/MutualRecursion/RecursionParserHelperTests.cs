using NUnit.Framework;
using Phantom.Parsers;
using Phantom.Parsers.Composite;
using Phantom.Parsers.Terminals;
using Phantom.Scanners;

namespace Phantom.Unit.Tests.MutualRecursion
{
	[TestFixture]
	public class RecursionParserHelperTests
	{
		private IParser subject;
		private IScanner valid_recursive, invalid_recursive, valid_simple, invalid_simple;

		[SetUp]
		public void a_recursive_parser_pattern()
		{
			// Build a parser for patterns of nested square brackets
			subject = Recursion.Over(recursive =>
			{
				var left = new LiteralCharacter('[');
				var right = new LiteralCharacter(']');

				var mrec = new Repetition(recursive, 0, int.MaxValue);
				var lrec = new Sequence(left, mrec);
				var lr = new Sequence(lrec, right);

				return lr;
			});

			valid_recursive = new ScanStrings("[[[][]]]");
			valid_simple = new ScanStrings("[]");
			invalid_recursive = new ScanStrings("[[[][[[]]]"); // unbalanced
			invalid_simple = new ScanStrings("hello, world");
		}

		[Test]
		public void should_be_build_a_recursion_parser()
		{
			Assert.That(subject, Is.InstanceOf<Recursion>());
		}

		[Test]
		public void it_should_pass_on_valid_recursive_input ()
		{
			var result = subject.Parse(valid_recursive);

			Assert.That(result.Success, Is.True);
			Assert.That(result.Value, Is.EqualTo("[[[][]]]"));
		}

		[Test]
		public void it_should_pass_on_valid_non_recursive_input ()
		{
			var result = subject.Parse(valid_simple);

			Assert.That(result.Success, Is.True);
			Assert.That(result.Value, Is.EqualTo("[]"));
		}

		[Test]
		public void it_should_fail_on_invalid_recursive_input ()
		{
			var result = subject.Parse(invalid_recursive);

			Assert.That(result.Success, Is.False);
		}

		[Test]
		public void it_should_fail_on_invalid_non_recursive_input ()
		{
			var result = subject.Parse(invalid_simple);

			Assert.That(result.Success, Is.False);
		}
	}
}

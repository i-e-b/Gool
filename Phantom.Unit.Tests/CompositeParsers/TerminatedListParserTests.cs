using System.Linq;
using NUnit.Framework;
using Phantom.Parsers;
using Phantom.Parsers.Composite;
using Phantom.Parsers.Terminals;
using Phantom.Scanners;

namespace Phantom.Unit.Tests.CompositeParsers
{
	[TestFixture]
	public class TerminatedListParserTests
	{
		IParser alpha, semi;
		IParser subject;

		[SetUp]
		public void semicolon_terminated_list ()
		{
			alpha = new RegularExpression("[a-zA-Z]+");
			semi = new LiteralCharacter(';');
			subject = new TerminatedList(alpha, semi);
		}

		[Test]
		public void single_item_without_terminator_passes ()
		{
			var scanner = new ScanStrings("one");
			var result = subject.Parse(scanner);

			Assert.That(result.Success, Is.True, "Result success");
			Assert.That(result.Value, Is.EqualTo("one"));
		}

		[Test]
		public void single_item_with_terminator_passes ()
		{
			var scanner = new ScanStrings("one;");
			var result = subject.Parse(scanner);

			Assert.That(result.Success, Is.True, "Result success");
			Assert.That(result.Value, Is.EqualTo("one;"));
		}

		[Test]
		public void multiple_items_with_end_terminator_missing_passes ()
		{
			var scanner = new ScanStrings("one;two");
			var result = subject.Parse(scanner);

			Assert.That(result.Success, Is.True, "Result success");
			Assert.That(result.Value, Is.EqualTo("one;two"));
		}

		[Test]
		public void multiple_items_with_end_terminator_passes ()
		{
			var scanner = new ScanStrings("one;two;");
			var result = subject.Parse(scanner);

			Assert.That(result.Success, Is.True, "Result success");
			Assert.That(result.Value, Is.EqualTo("one;two;"));
		}

		[Test]
		public void terminator_only_fails ()
		{
			var scanner = new ScanStrings(";");
			var result = subject.Parse(scanner);

			Assert.That(result.Success, Is.False, "Result success");
		}

		[Test]
		public void child_matches_of_terminated_list_are_the_separated_items ()
		{
			var scanner = new ScanStrings("one;two;three");
			var expectedItems = new[] {"one",";","two",";","three"};
			var result = subject.Parse(scanner);

			Assert.That(result.Success, Is.True, "Result success");
			Assert.That(result.ChildMatches.Select(p=>p.Value), Is.EquivalentTo(expectedItems));
		}

		[Test]
		public void double_terminator_matches_single_item_only()
		{
			var scanner = new ScanStrings("one;;");
			var result = subject.Parse(scanner);

			Assert.That(result.Success, Is.True, "Result success");
			Assert.That(result.Value, Is.EqualTo("one;"));
		}
	}
}

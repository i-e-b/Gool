using System.Linq;
using NUnit.Framework;
using Phantom.Parsers.Composite;
using Phantom.Parsers.Terminals;
using Phantom.Scanners;

namespace Phantom.Unit.Tests.CompositeParsers
{
	[TestFixture]
	public class DelimitedListParserTests
	{
		private IParser alpha, semi;
		private IParser subject;

		[SetUp]
		public void semicolon_delimited_list ()
		{
			alpha = new RegularExpression("[a-zA-Z]+");
			semi = new LiteralCharacter(';');
			subject = new DelimitedList(alpha, semi);
		}

		[Test]
		public void single_item_without_delimiter_passes ()
		{
			var scanner = new ScanStrings("one");
			var result = subject.Parse(scanner);
			
			Assert.That(result.Success, Is.True, "Result success");
			Assert.That(result.Value, Is.EqualTo("one"));
		}

		[Test]
		public void single_item_with_delimiter_at_end_passes_but_does_not_find_delimiter ()
		{
			var scanner = new ScanStrings("one;");
			var result = subject.Parse(scanner);

			Assert.That(result.Success, Is.True, "Result success");
			Assert.That(result.Value, Is.EqualTo("one"));
		}

		[Test]
		public void multiple_items_with_separated_by_delimiter_are_all_found ()
		{
			var scanner = new ScanStrings("one;two");
			var result = subject.Parse(scanner);

			Assert.That(result.Success, Is.True, "Result success");
			Assert.That(result.Value, Is.EqualTo("one;two"));
		}

		[Test]
		public void multiple_items_with_end_delimiter_passes_but_does_not_match_end_delimiter ()
		{
			var scanner = new ScanStrings("one;two;");
			var result = subject.Parse(scanner);

			Assert.That(result.Success, Is.True, "Result success");
			Assert.That(result.Value, Is.EqualTo("one;two"));
		}

		[Test]
		public void delimiter_only_fails ()
		{
			var scanner = new ScanStrings(";");
			var result = subject.Parse(scanner);

			Assert.That(result.Success, Is.False, "Result success");
		}

		[Test]
		public void child_matches_are_the_separated_items ()
		{
			var scanner = new ScanStrings("one;two;three;");
			var expectedItems = new[] { "one", ";", "two", ";", "three" };
			var result = subject.Parse(scanner);

			Assert.That(result.Success, Is.True, "Result success");
			Assert.That(result.ChildMatches.Select(p=>p.Value), Is.EquivalentTo(expectedItems));
		}

		[Test]
		public void double_delimiter_matches_single_item_only_without_delimited()
		{
			var scanner = new ScanStrings("one;;");
			var result = subject.Parse(scanner);

			Assert.That(result.Success, Is.True, "Result success");
			Assert.That(result.Value, Is.EqualTo("one"));
		}
	}
}

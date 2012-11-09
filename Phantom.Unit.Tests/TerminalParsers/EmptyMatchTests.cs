using NUnit.Framework;
using Phantom.Parsers;
using Phantom.Parsers.Terminals;
using Phantom.Scanners;

namespace Phantom.Unit.Tests.TerminalParsers
{
	[TestFixture]
	public class EmptyMatchTests
	{
		IScanner scanner;
		IParser subject;
		const string Input = "This is my input";

		[SetUp]
		public void a_string_scanner_with_some_text ()
		{
			scanner = new ScanStrings(Input);
			subject = new EmptyMatch();
		}
		
		[Test]
		public void empty_parser_always_succeeds ()
		{
			var result = subject.TryMatch(scanner);
			Assert.IsTrue(result.Success);
		}

		[Test]
		public void result_length_is_always_zero ()
		{
			var result = subject.TryMatch(scanner);
			Assert.That(result.Length, Is.EqualTo(0));
		}

		[Test]
		public void result_offset_matches_scanner_offset ()
		{
			for (int i = 0; i < 10; i++)
			{
				scanner.Offset = i;
				var result = subject.TryMatch(scanner);
				Assert.That(result.Offset, Is.EqualTo(i));
			}
		}

		[Test]
		public void result_parser_source_is_the_subject_parser ()
		{
			var result = subject.TryMatch(scanner);
			Assert.That(result.SourceParser, Is.EqualTo(subject));
		}

		[Test]
		public void scanner_offset_does_not_change_after_match ()
		{
			var before = scanner.Offset;
			subject.TryMatch(scanner);
			var after = scanner.Offset;
			Assert.That(after, Is.EqualTo(before));
		}
	}
}

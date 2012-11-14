using NUnit.Framework;
using Phantom.Parsers;
using Phantom.Parsers.Interfaces;
using Phantom.Parsers.Terminals;
using Phantom.Scanners;

namespace Phantom.Unit.Tests.TerminalParsers
{
	[TestFixture]
	public class LiteralStringTests
	{
		IScanner scanner;
		IMatchingParser subject;
		const string Input = "This is my input";
		readonly int[] matchOffsets = new[]{2, 5};

		[SetUp]
		public void a_string_scanner_with_some_text ()
		{
			scanner = new ScanStrings(Input);
			subject = new LiteralString("is");
		}

		[Test]
		public void successful_if_current_position_has_same_substring_as_target ()
		{
			scanner.Offset = 2;
			var result = subject.TryMatch(scanner);
			Assert.That(result.Success, Is.True);
		}

		[Test]
		public void fails_if_current_position_is_different_character_to_target ()
		{
			scanner.Offset = 3;
			var result = subject.TryMatch(scanner);
			Assert.That(result.Success, Is.False);
		}

		[Test]
		public void successful_result_length_is_always_equal_to_target_substring ()
		{
			foreach (var offset in matchOffsets)
			{
				scanner.Offset = offset;
				var result = subject.TryMatch(scanner);
				Assert.That(result.Length, Is.EqualTo(2));
			}
		}

		[Test]
		public void fails_at_end_of_input ()
		{
			scanner.Offset = Input.Length - 1;
			scanner.Read();

			var result = subject.TryMatch(scanner);
			Assert.IsFalse(result.Success);
		}

		[Test]
		public void scanner_offset_advances_one_position_after_match ()
		{
			scanner.Offset = 2;
			subject.TryMatch(scanner);
			Assert.That(scanner.Offset, Is.EqualTo(4));
		}

		[Test]
		public void scanner_offset_is_unchanged_after_non_match ()
		{
			scanner.Offset = 1;
			subject.TryMatch(scanner);
			Assert.That(scanner.Offset, Is.EqualTo(1));
		}
		
		[Test]
		public void captures_source_substring ()
		{
			scanner.Offset = 2;
			var result = subject.TryMatch(scanner);
			Assert.That(result.Value, Is.EqualTo("is"));
		}
	}
}

using NUnit.Framework;
using Phantom.Parsers;
using Phantom.Parsers.Interfaces;
using Phantom.Parsers.Terminals;
using Phantom.Scanners;

namespace Phantom.Unit.Tests.TerminalParsers
{
	[TestFixture]
	public class LiteralCharacterTests
	{
		IScanner scanner;
		IMatchingParser subject;
		const string Input = "This is my input";
		readonly int[] matchOffsets = new[]{2, 5, 11};

		[SetUp]
		public void a_string_scanner_with_some_text ()
		{
			scanner = new ScanStrings(Input);
			subject = new LiteralCharacter('i');
		}

		[Test]
		public void successful_if_current_position_is_same_character_as_target ()
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
		public void successful_result_length_is_always_one ()
		{
			foreach (var offset in matchOffsets)
			{
				scanner.Offset = offset;
				var result = subject.TryMatch(scanner);
				Assert.That(result.Length, Is.EqualTo(1));
			}
		}

		[Test]
		public void fails_at_end_of_input ()
		{
			scanner.Offset = Input.Length - 1;
			scanner.Read();

			var result = subject.TryMatch(scanner);
			Assert.That(result.Success, Is.False);
		}

		[Test]
		public void scanner_offset_advances_one_position_after_match ()
		{
			scanner.Offset = 2;
			subject.TryMatch(scanner);
			Assert.That(scanner.Offset, Is.EqualTo(3));
		}

		[Test]
		public void scanner_offset_is_unchanged_after_non_match ()
		{
			scanner.Offset = 1;
			subject.TryMatch(scanner);
			Assert.That(scanner.Offset, Is.EqualTo(1));
		}
	}
}

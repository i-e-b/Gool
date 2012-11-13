using NUnit.Framework;
using Phantom.Parsers;
using Phantom.Parsers.Terminals;
using Phantom.Scanners;

namespace Phantom.Unit.Tests.TerminalParsers
{
	[TestFixture]
	public class AnyCharacterTests
	{
		IScanner scanner;
		ITerminal subject;
		const string Input = "This is my input";

		[SetUp]
		public void a_string_scanner_with_some_text ()
		{
			scanner = new ScanStrings(Input);
			subject = new AnyCharacter();
		}

		[Test]
		public void returns_current_scanner_character ()
		{
			Assert.That(subject.TryMatch(scanner).Value, Is.EqualTo("T"));
			Assert.That(subject.TryMatch(scanner).Value, Is.EqualTo("h"));
			Assert.That(subject.TryMatch(scanner).Value, Is.EqualTo("i"));
			Assert.That(subject.TryMatch(scanner).Value, Is.EqualTo("s"));
			Assert.That(subject.TryMatch(scanner).Value, Is.EqualTo(" "));
			Assert.That(subject.TryMatch(scanner).Value, Is.EqualTo("i"));
			Assert.That(subject.TryMatch(scanner).Value, Is.EqualTo("s"));
			Assert.That(subject.TryMatch(scanner).Value, Is.EqualTo(" "));
			Assert.That(subject.TryMatch(scanner).Value, Is.EqualTo("m"));
			Assert.That(subject.TryMatch(scanner).Value, Is.EqualTo("y"));
			Assert.That(subject.TryMatch(scanner).Value, Is.EqualTo(" "));
			Assert.That(subject.TryMatch(scanner).Value, Is.EqualTo("i"));
			Assert.That(subject.TryMatch(scanner).Value, Is.EqualTo("n"));
			Assert.That(subject.TryMatch(scanner).Value, Is.EqualTo("p"));
			Assert.That(subject.TryMatch(scanner).Value, Is.EqualTo("u"));
			Assert.That(subject.TryMatch(scanner).Value, Is.EqualTo("t"));
			Assert.That(subject.TryMatch(scanner).Success, Is.False);
		}

		[Test]
		public void any_character_parser_succeeds_if_there_is_a_character_available ()
		{
			for (int i = 0; i < Input.Length; i++)
			{
				var result = subject.TryMatch(scanner);
				Assert.IsTrue(result.Success);
			}
		}
		
		[Test]
		public void successful_result_length_is_always_one ()
		{
			for (int i = 0; i < Input.Length; i++)
			{
				var result = subject.TryMatch(scanner);
				Assert.That(result.Length, Is.EqualTo(1));
			}
		}

		[Test]
		public void any_character_parser_fails_at_end_of_input ()
		{
			scanner.Offset = Input.Length - 1;
			scanner.Read();

			var result = subject.TryMatch(scanner);
			Assert.IsFalse(result.Success);
		}

		[Test]
		public void scanner_offset_advances_one_position_after_match ()
		{
			var before = scanner.Offset;
			subject.TryMatch(scanner);
			var after = scanner.Offset;
			Assert.That(after, Is.EqualTo(before + 1));
		}
	}
}

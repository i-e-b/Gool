using NUnit.Framework;
using Phantom.Parsers;
using Phantom.Parsers.Interfaces;
using Phantom.Parsers.Terminals;
using Phantom.Scanners;

namespace Phantom.Unit.Tests.TerminalParsers
{
	[TestFixture]
	public class EndOfLineTests
	{
		IScanner unix, windows, oldMac, twoUnix, twoWindows, mangledTwo;
		IMatchingParser subject;
		string oneLineInput = "Just some normal input";
		ScanStrings oneLineScanner;

		[SetUp]
		public void a_string_scanner_with_some_text()
		{
			// Newline is char 3+
			oneLineScanner = new ScanStrings(oneLineInput);
			unix = new ScanStrings("-> \n <-");
			windows = new ScanStrings("-> \r\n <-");
			oldMac = new ScanStrings("-> \r <-");
			twoUnix = new ScanStrings("-> \n\n <-");
			twoWindows = new ScanStrings("-> \r\n\r\n <-");
			mangledTwo = new ScanStrings("-> \n\r <-");
			subject = new EndOfLine();
		}

		[Test]
		public void finds_the_correct_number_of_newlines()
		{
			Assert.That(CountLineEnds(unix), Is.EqualTo(1), "Unix");
			Assert.That(CountLineEnds(windows), Is.EqualTo(1), "Windows");
			Assert.That(CountLineEnds(oldMac), Is.EqualTo(1), "Old Mac");
			Assert.That(CountLineEnds(twoUnix), Is.EqualTo(2), "Two Unix");
			Assert.That(CountLineEnds(twoWindows), Is.EqualTo(2), "Two Windows");
			Assert.That(CountLineEnds(mangledTwo), Is.EqualTo(2), "Mixed type, not Windows order");
		}

		[Test]
		public void advances_one_position_over_non_windows_linebreaks()
		{
			unix.Offset = 3;
			subject.TryMatch(unix);
			Assert.That(unix.Offset, Is.EqualTo(4));
		}

		[Test]
		public void advances_one_position_over_windows_linebreaks()
		{
			windows.Offset = 3;
			subject.TryMatch(windows);
			Assert.That(windows.Offset, Is.EqualTo(5));
		}
		
		[Test]
		public void captures_source_linebreak_block ()
		{
			unix.Offset = 3;
			var unixResult = subject.TryMatch(unix);
			Assert.That(unixResult.Value, Is.EqualTo("\n"));
			
			windows.Offset = 3;
			var windowsResult = subject.TryMatch(windows);
			Assert.That(windowsResult.Value, Is.EqualTo("\r\n"));
		}

		[Test]
		public void no_match_on_non_linebreak_characters ()
		{
			var result = subject.TryMatch(unix);
			Assert.IsFalse(result.Success);
		}

		[Test]
		public void no_match_on_end_of_input ()
		{
			oneLineScanner.Offset = oneLineInput.Length -1;
			oneLineScanner.Read();

			var result = subject.TryMatch(oneLineScanner);

			Assert.That(result.Success, Is.False);
		}

		int CountLineEnds(IScanner scanner)
		{
			int count = 0;
			while (!scanner.EndOfInput)
			{
				if ( ! subject.TryMatch(scanner).Success) scanner.Read();
				else count++;
			}
			return count;
		}
	}
}

using NUnit.Framework;
using Phantom.Scanners;

namespace Phantom.Unit.Tests.Scanners
{
	[TestFixture]
	public class StringScanner_Normalising_without_whitespace_skipped
	{                       //0         1            2         3
		private IScanner subject;   //012345678901 2 3 45678901234567890
		private const string Input = "this is my \r\n\t boom-stick!    ";

		[SetUp]
		public void setup()
		{
			subject = new ScanStrings(Input){SkipWhitespace = false};
		}

		[Test]
		public void Normalising_on_non_whitespace_character_has_no_effect ()
		{
			subject.Seek(2);
			subject.Normalise();

			Assert.That(subject.Offset, Is.EqualTo(2));
		}

		[Test]
		public void Normalising_on_whitespace_character_has_no_effect ()
		{
			subject.Seek(10);
			subject.Normalise();

			Assert.That(subject.Offset, Is.EqualTo(10));
		}
		
		[Test]
		public void Normalising_on_end_padding_whitespace_character_has_no_effect ()
		{
			subject.Seek(26);
			subject.Normalise();

			Assert.That(subject.Offset, Is.EqualTo(26));
		}
	}
}

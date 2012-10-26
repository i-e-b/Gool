using NUnit.Framework;
using Phantom.Scanners;

namespace Phantom.Unit.Tests.Scanners {
	[TestFixture]
	public class TransformingStreamsToLowerCase {
		[Test]
		[TestCase("all lower case", "all lower case")]
		[TestCase("Mixed Case!", "mixed case!")]
		[TestCase("UTF-8 test: ß À Á Â Ã Ä Å Æ Ç È É Ê Ë Ì Í Î Ï Ð Ñ Ò Ó Ô Õ Ö",
			      "utf-8 test: ß à á â ã ä å æ ç è é ê ë ì í î ï ð ñ ò ó ô õ ö")]
		public void Lowercases_normal_characters (string input, string expected) {
			var scanner = new ScanStrings(input);
			scanner.Transform = new TransformToLower();

			var result = scanner.Substring(0, input.Length);

			Assert.That(result, Is.EqualTo(expected));
		}
	}
}

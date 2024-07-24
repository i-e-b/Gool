using System;
using NUnit.Framework;
using Phantom.Scanners;
using SampleGrammars;

namespace Phantom.Integration.Tests
{
	[TestFixture]
	public class XmlTests
	{
		const string sample = 
@"<note>
	<to>Tove</to>
	<from>Jani</from>
	<heading>Reminder</heading>
	<body>Don't forget me this weekend!</body>
</note>";
		
		const string brokenSample = 
@"<note>
	<to>Tove</to>
	<from>Jani</from>
	<heading>Reminder</broken>
	<body>Don't forget me this weekend!</body>
</wrong>";

		[Test]
		public void XmlDocumentParsesSuccessfully()
		{
			var parser = new XMLParser().TheParser;
			var scanner = new ScanStrings(sample);

			var result = parser.Parse(scanner);
			
			Assert.That(result.Success, Is.True, result + ": " + result.Value);
			Assert.That(result.Value, Is.EqualTo(sample));

			foreach (var match in result.BottomLevelMatches())
			{
				Console.WriteLine(match.Value);
			}
		}
		
		[Test]
		public void WellStructuredButInvalidXmlDocumentParsesSuccessfully()
		{
			var parser = new XMLParser().TheParser;
			var scanner = new ScanStrings(brokenSample);

			var result = parser.Parse(scanner);

			Assert.That(result.Success, Is.True, result + ": " + result.Value);
			Assert.That(result.Value, Is.EqualTo(brokenSample));
		}
	}
}

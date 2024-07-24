using System;
using NUnit.Framework;
using Phantom.Scanners;
using SampleGrammars;

namespace Phantom.Integration.Tests
{
	[TestFixture]
	public class XmlTests
	{
		private const string Sample = 
@"<note>
	<to>Tove</to>
	<from>Jani</from>
	<heading>Reminder</heading>
	<body>Don't forget me this weekend!</body>
	<empty></empty>
</note>";

		private const string BrokenSample = 
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
			var scanner = new ScanStrings(Sample);

			var result = parser.Parse(scanner);
			
			Assert.That(result.Success, Is.True, result + ": " + result.Value);
			Assert.That(result.Value, Is.EqualTo(Sample));

			foreach (var match in result.BottomLevelMatches())
			{
				Console.WriteLine(match.Value + " -> " + match.SourceParser);
			}
		}
		
		[Test]
		public void WellStructuredButInvalidXmlDocumentParsesSuccessfully()
		{
			var parser = new XMLParser().TheParser;
			var scanner = new ScanStrings(BrokenSample);

			var result = parser.Parse(scanner);

			Assert.That(result.Success, Is.True, result + ": " + result.Value);
			Assert.That(result.Value, Is.EqualTo(BrokenSample));

			foreach (var match in result.DepthFirstWalk())
			{
				var tag = match.SourceParser?.GetTag();
				if (tag is null) continue;
				if (string.IsNullOrWhiteSpace(match.Value)) continue;
				
				Console.WriteLine(match.Value + " : " + tag);
			}
		}
	}
}

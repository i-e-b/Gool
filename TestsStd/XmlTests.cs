using System.Diagnostics;
using NUnit.Framework;
using Phantom.Scanners;
using Samples;

namespace TestsStd;

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
        @"<note type=""private"" class=""sheer"">
	<to>Tove</to>
	<from>Jani</from>
	<heading>Reminder</broken>
	<body type=""text"">Don't forget me this weekend!</body>
</wrong>";


    [Test]
    public void XmlDocumentParsesSuccessfully()
    {
        var parser = new XmlParser().TheParser;
        var scanner = new ScanStrings(Sample);

        var sw = new Stopwatch();
        sw.Start();
        var result = parser.Parse(scanner);
        sw.Stop();
        Console.WriteLine($"Parsing took {sw.Elapsed.TotalMicroseconds} µs");

        // Faults: Something is advancing too far?

        foreach (var failPoint in scanner.ListFailures()) Console.WriteLine(failPoint);

        Assert.That(result.Success, Is.True, result + ": " + result.Value);
        Assert.That(result.Value, Is.EqualTo(Sample));

        foreach (var match in result.BottomLevelMatches())
        {
            Console.WriteLine(match.Description());
        }
    }

    [Test, Description("This demonstrates that long-distance relationships between tokens are not expressed in the parser")]
    public void WellStructuredButInvalidXmlDocumentParsesSuccessfully()
    {
        var parser = new XmlParser().TheParser;
        var scanner = new ScanStrings(BrokenSample);

        var sw = new Stopwatch();
        sw.Start();
        var result = parser.Parse(scanner);
        sw.Stop();
        Console.WriteLine($"Parsing took {sw.Elapsed.TotalMicroseconds} µs");

        foreach (var fail in scanner.ListFailures())
        {
            Console.WriteLine(fail);
        }

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


    [Test]
    public void ConvertingParsedXmlTokensIntoStructure()
    {
        var parser = new XmlParser().TheParser;
        var scanner = new ScanStrings(Sample);

        var result = parser.Parse(scanner);

        foreach (var fail in scanner.ListFailures())
        {
            Console.WriteLine(fail);
        }

        Assert.That(result.Success, Is.True, result + ": " + result.Value);
        Assert.That(result.Value, Is.EqualTo(Sample));

        var taggedTokens = result.TaggedTokens();

        foreach (var token in taggedTokens)
        {
            switch (token.Tag)
            {
                case XmlParser.Text:
                    if (!string.IsNullOrWhiteSpace(token.Value)) Console.WriteLine("content: " + token.Value);
                    break;

                case XmlParser.OpenTag:
                    Console.WriteLine(token.ChildrenWithTag(XmlParser.TagId).FirstOrDefault()?.Value + "{");
                    break;

                case XmlParser.CloseTag:
                    Console.WriteLine("}" + token.ChildrenWithTag(XmlParser.TagId).FirstOrDefault()?.Value);
                    break;
            }
        }
    }
}
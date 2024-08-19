using System.Diagnostics;
using System.Text;
using NUnit.Framework;
using Phantom.Results;
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
        var sw = new Stopwatch();
        sw.Start();
        var result = XmlExample.Parser.ParseString(Sample);
        sw.Stop();
        Console.WriteLine($"Parsing took {sw.Elapsed.TotalMicroseconds} µs");

        // Faults: Something is advancing too far?

        foreach (var failPoint in result.Scanner.ListFailures()) Console.WriteLine(failPoint);

        Assert.That(result.Success, Is.True, result + ": " + result.Value);
        Assert.That(result.Value, Is.EqualTo(Sample));

        foreach (var match in result.BottomLevelMatchesDepthFirst())
        {
            Console.WriteLine(match.Description());
        }
    }

    [Test, Description("This demonstrates that long-distance relationships between tokens are not expressed in the parser")]
    public void WellStructuredButInvalidXmlDocumentParsesSuccessfully()
    {
        var sw = new Stopwatch();
        sw.Start();
        var result = XmlExample.Parser.ParseString(BrokenSample);
        sw.Stop();
        Console.WriteLine($"Parsing took {sw.Elapsed.TotalMicroseconds} µs");

        foreach (var fail in result.Scanner.ListFailures())
        {
            Console.WriteLine(fail);
        }

        Assert.That(result.Success, Is.True, result + ": " + result.Value);
        Assert.That(result.Value, Is.EqualTo(BrokenSample));

        foreach (var match in result.DepthFirstWalk())
        {
            var tag = match.SourceParser?.Tag;
            if (tag is null) continue;
            if (string.IsNullOrWhiteSpace(match.Value)) continue;

            Console.WriteLine(match.Value + " : " + tag);
        }
    }
    
    [Test, Description("This demonstrates that long-distance relationships between tokens are not expressed in the parser")]
    public void can_detect_tag_mismatches_in_scoped_tree()
    {
        var sw = new Stopwatch();
        sw.Start();
        var result = XmlExample.Parser.ParseString(BrokenSample);
        sw.Stop();
        Console.WriteLine($"Parsing took {sw.Elapsed.TotalMicroseconds} µs");

        var tree = ScopeNode.FromMatch(result);
        var errors = new List<string>();
        
        tree.BreadthFirstWalk(n =>
        {
            if (n.NodeType == ScopeNodeType.ScopeChange)
            {
                var open = n.OpeningMatch?.FindTag(XmlExample.TagId)?.Value;
                var close = n.ClosingMatch?.FindTag(XmlExample.TagId)?.Value;
                if (open != close) errors.Add($"<{open}> does not match </{close}>");
            }
        });

        Assert.That(errors, Contains.Item("<note> does not match </wrong>"));
        Assert.That(errors, Contains.Item("<heading> does not match </broken>"));
    }


    [Test]
    public void ConvertingParsedXmlTokensIntoStructure()
    {
        var result = XmlExample.Parser.ParseString(Sample);

        foreach (var fail in result.Scanner.ListFailures())
        {
            Console.WriteLine(fail);
        }

        Assert.That(result.Success, Is.True, result + ": " + result.Value);
        Assert.That(result.Value, Is.EqualTo(Sample));

        var taggedTokens = result.TaggedTokensDepthFirst();

        var sb = new StringBuilder();
        foreach (var token in taggedTokens)
        {
            switch (token.Tag)
            {
                case XmlExample.Text:
                    if (!string.IsNullOrWhiteSpace(token.Value)) sb.Append($"[{token.Value}]");
                    break;

                case XmlExample.OpenTag:
                    sb.Append(token.ChildrenWithTag(XmlExample.TagId).FirstOrDefault()?.Value + "{");
                    break;

                case XmlExample.CloseTag:
                    sb.AppendLine("}" + token.ChildrenWithTag(XmlExample.TagId).FirstOrDefault()?.Value);
                    break;
            }
        }

        Console.WriteLine(sb.ToString());
        Assert.That(sb.ToString(), Is.EqualTo(@"note{to{[Tove]}to
from{[Jani]}from
heading{[Reminder]}heading
body{[Don't forget me this weekend!]}body
empty{}empty
}note
"));
    }
}
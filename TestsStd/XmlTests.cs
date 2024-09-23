﻿using System.Diagnostics;
using System.Text;
using Gool.Results;
using NUnit.Framework;
using Samples;

namespace TestsStd;

[TestFixture]
public class XmlTests
{
    private const string SimpleXmlSample =
        @"<note>
	<to>Tove</to>
	<from>Jani</from>
	<heading>Reminder</heading>
	<body>Don't forget me this weekend!</body>
	<empty></empty>
</note>";

    private const string BrokenSimpleXmlSample =
        @"<note type=""private"" class=""sheer"">
	<to>Tove</to>
	<from>Jani</from>
	<heading>Reminder</broken>
	<body type=""text"">Don't forget me this weekend!</body>
</wrong>";

    private const string FullXmlBasicDoc =
        """
        <?xml version="1.0" encoding="UTF-8"?>
        <note>
          <to>Tove</to>
          <from>Jani</from>
          <heading>Reminder</heading>
          <body>Don't forget me this weekend!</body>
        </note>
        """;

    private const string FullXmlDocTypeAndStyleSheetDoc =
        """
        <?xml version="1.0" encoding="UTF-8"?>
        <!DOCTYPE spec SYSTEM "xmlspec.dtd">
        <?xml-stylesheet type="text/xsl" href="REC-xml.xsl"?>
        <note>
          <to>Tove</to>
          <from>Jani</from>
          <heading>Reminder</heading>
          <body>Don't forget me this weekend!</body>
        </note>
        """;

    [Test]
    public void SimpleXmlDocumentParsesSuccessfully()
    {
        var sw = new Stopwatch();
        sw.Start();
        var result = XmlExample.SimpleXmlParser.ParsePartialString(SimpleXmlSample);
        sw.Stop();
        Console.WriteLine($"Parsing took {sw.Elapsed.TotalMicroseconds} µs");

        // Faults: Something is advancing too far?

        foreach (var failPoint in result.Scanner.ListFailures()) Console.WriteLine(failPoint);

        Assert.That(result.Success, Is.True, result + ": " + result.Value);
        Assert.That(result.Value, Is.EqualTo(SimpleXmlSample));

        foreach (var match in result.BottomLevelMatchesDepthFirst())
        {
            Console.WriteLine(match.Description());
        }
    }

    [Test, Description("This demonstrates that long-distance relationships between tokens are not expressed in the parser")]
    public void WellStructuredButInvalidSimpleXmlDocumentParsesSuccessfully()
    {
        var sw = new Stopwatch();
        sw.Start();
        var result = XmlExample.SimpleXmlParser.ParsePartialString(BrokenSimpleXmlSample);
        sw.Stop();
        Console.WriteLine($"Parsing took {sw.Elapsed.TotalMicroseconds} µs");

        foreach (var fail in result.Scanner.ListFailures())
        {
            Console.WriteLine(fail);
        }

        Assert.That(result.Success, Is.True, result + ": " + result.Value);
        Assert.That(result.Value, Is.EqualTo(BrokenSimpleXmlSample));

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
        var parser = XmlExample.SimpleXmlParser;
        var sw     = new Stopwatch();
        sw.Start();
        var result = parser.ParsePartialString(BrokenSimpleXmlSample);
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
        var result = XmlExample.SimpleXmlParser.ParsePartialString(SimpleXmlSample);

        foreach (var fail in result.Scanner.ListFailures())
        {
            Console.WriteLine(fail);
        }

        Assert.That(result.Success, Is.True, result + ": " + result.Value);
        Assert.That(result.Value, Is.EqualTo(SimpleXmlSample));

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
        Assert.That(sb.ToString().Replace("\r",""), Is.EqualTo(@"note{to{[Tove]}to
from{[Jani]}from
heading{[Reminder]}heading
body{[Don't forget me this weekend!]}body
empty{}empty
}note
".Replace("\r","")));
    }


    [Test]
    public void can_parse_a_basic_full_xml_document_with_the_full_specification_parser()
    {
        var sw     = Stopwatch.StartNew();
        var parser = XmlExample.FullXmlParser();
        sw.Stop();
        Console.WriteLine($"Constructing parser took {sw.Elapsed.TotalMicroseconds} µs");

        sw.Restart();
        var result = parser.ParsePartialString(FullXmlBasicDoc);
        sw.Stop();
        Console.WriteLine($"Parsing took {sw.Elapsed.TotalMicroseconds} µs");

        foreach (var failPoint in result.Scanner.ListFailures()) Console.WriteLine(failPoint);

        Assert.That(result.Success, Is.True, result + ": " + result.Value);
        Assert.That(result.Value, Is.EqualTo(FullXmlBasicDoc));

        var tree = ScopeNode.FromMatch(result);
        PrintRecursive(tree, 0);
    }

    [Test]
    public void can_parse_a_fully_defined_xml_document_with_the_full_specification_parser()
    {
        var sw     = Stopwatch.StartNew();
        var parser = XmlExample.FullXmlParser();
        sw.Stop();
        Console.WriteLine($"Constructing parser took {sw.Elapsed.TotalMicroseconds} µs");

        sw.Restart();
        var result = parser.ParsePartialString(FullXmlDocTypeAndStyleSheetDoc);
        sw.Stop();
        Console.WriteLine($"Parsing took {sw.Elapsed.TotalMicroseconds} µs");

        foreach (var failPoint in result.Scanner.ListFailures()) Console.WriteLine(failPoint);

        Assert.That(result.Success, Is.True, result + ": " + result.Value);
        Assert.That(result.Value, Is.EqualTo(FullXmlDocTypeAndStyleSheetDoc));

        var tree = ScopeNode.FromMatch(result);
        PrintRecursive(tree, 0);
    }


    [Test(Description = "This attempts to parse the entire XML spec, defined in XML (from https://www.w3.org/TR/2008/REC-xml-20081126/REC-xml-20081126.xml )")]
    public void can_parse_a_huge_and_complicated_xml_document_with_the_full_specification_parser()
    {
        var sw     = Stopwatch.StartNew();
        var parser = XmlExample.FullXmlParser();
        sw.Stop();
        Console.WriteLine($"Constructing parser took {sw.Elapsed.TotalMicroseconds} µs");

        var input = File.ReadAllText(@"Samples/xml_spec.xml");

        sw.Restart();
        var result = parser.ParseEntireString(input);
        sw.Stop();
        Console.WriteLine($"Parsing took {sw.Elapsed.TotalMilliseconds} ms");

        foreach (var failPoint in result.Scanner.ListFailures()) Console.WriteLine(failPoint);

        Assert.That(result.Success, Is.True, result + ": " + result.Value);
        Assert.That(result.Value, Is.EqualTo(input));

        sw.Restart();
        var tree = ScopeNode.FromMatch(result);
        sw.Stop();
        Console.WriteLine($"Reinterpreting parser tree took {sw.Elapsed.TotalMilliseconds} ms");

        PrintRecursive(tree, 0);
    }




    private static void PrintRecursive(ScopeNode node, int indent)
    {
        switch (node.NodeType)
        {
            case ScopeNodeType.Root:
                Console.WriteLine("Document");
                if (node.OpeningMatch is not null || node.ClosingMatch is not null) Console.WriteLine("Unbalanced scopes!");
                break;
            case ScopeNodeType.Data:
                // Hide junk
                if (node.DataMatch?.Tag == XmlExample.WhitespaceTag || string.IsNullOrWhiteSpace(node.DataMatch?.Value)) { break; }

                Console.WriteLine($"{I(indent)}[{node.DataMatch?.Tag}]: {node.DataMatch?.Value.Trim()}");
                break;
            case ScopeNodeType.ScopeChange:
            {
                if (node.OpeningMatch is not null)
                {
                    var match = node.OpeningMatch;
                    Console.WriteLine($"{I(indent + 1)}{match?.Value.Trim()}");
                }

                break;
            }
            default:
                Assert.Fail($"Node does not have a valid type: {node}");
                break;
        }

        foreach (var childNode in node.Children)
        {
            PrintRecursive(childNode, indent + 2);
        }

        if (node.ClosingMatch is not null)
        {
            var match = node.ClosingMatch;
            Console.WriteLine($"{I(indent + 1)}{match?.Value.Trim()}");
        }
    }

    private static string I(int indent)
    {
        if (indent < 0) indent = 0;
        return new string(' ', indent * 2);
    }
}
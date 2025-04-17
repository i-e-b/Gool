using System.Diagnostics;
using Gool;
using Gool.Parsers;
using Gool.Results;
using Gool.Scanners;
using NUnit.Framework;

namespace TestsStd;

[TestFixture]
public class TagAndScopeBasicTests
{
    private static IParser MultiTagSample()
    {
        // deliberately ambiguous grammar
        BNF
            a = 'a',
            b = 'b';

        BNF.TagAll("tag", a, b);

        return -( a | b );
    }

    [Test]
    public void multi_tagging()
    {
        const string sample  = "abba";
        var          parser  = MultiTagSample();
        var          scanner = new ScanStrings(sample);

        var sw = new Stopwatch();
        sw.Start();
        var result = parser.Parse(scanner);
        sw.Stop();
        Console.WriteLine($"Parsing took {sw.Elapsed.TotalMicroseconds} µs");

        var taggedItems = 0;
        foreach (var match in result.TaggedTokensDepthFirst())
        {
            taggedItems++;
            Console.Write(match.Value);
            Console.Write(" ");
        }

        Assert.That(result.Success, Is.True, result + ": " + result.Value);
        Assert.That(taggedItems, Is.EqualTo(4));
    }



    private static IParser TagAndScopeSample()
    {
        // deliberately ambiguous grammar
        BNF
            c = 'c',
            a = 'a' > !c.TreeScope(),
            b = 'b';

        BNF.TagAll("tag", a, b);

        return -( a | b );
    }

    [Test]
    public void scopes_versus_tags()
    {
        const string sample  = "abacab";
        var          parser  = TagAndScopeSample();
        var          scanner = new ScanStrings(sample);

        var sw = new Stopwatch();
        sw.Start();
        var result = parser.Parse(scanner);
        sw.Stop();
        Console.WriteLine($"Parsing took {sw.Elapsed.TotalMicroseconds} µs");
        Assert.That(result.Success, Is.True, result + ": " + result.Value);

        Console.WriteLine("Tagged items:");
        var taggedItems = 0;
        foreach (var match in result.TaggedTokensDepthFirst())
        {
            taggedItems++;
            Console.Write(match.Value);
            Console.Write(" ");
        }

        Assert.That(taggedItems, Is.EqualTo(5));

        var metaItems = 0;
        Console.WriteLine("\r\nAll items:");
        foreach (var match in result.DepthFirstWalk())
        {
            if (string.IsNullOrEmpty(match.Tag) && match.Scope == ScopeType.None) continue;

            metaItems++;
            Console.Write($"{match.Value} [{match.Tag},{match.Scope}] ");
        }
        Assert.That(metaItems, Is.EqualTo(6));
    }
}
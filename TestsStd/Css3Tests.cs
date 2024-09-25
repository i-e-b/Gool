using System.Diagnostics;
using Gool.Parsers;
using NUnit.Framework;
using Samples;
using TestsStd.Helpers;

namespace TestsStd;

[TestFixture]
public class Css3Tests
{
    private const string BasicSample =
        """
            /* Style for a "Candidate Recommendation Draft" */
        
            @import "base.css";
        
            body {
              background-image: url(logos/CRD.svg);
            }
        """;

    [Test, Ignore("This parser doesn't work properly")]
    public void can_parse_basic_css_file()
    {
        var sw     = Stopwatch.StartNew();
        var parser = Css3Example.Css3_W3C();
        sw.Stop();
        Console.WriteLine($"Creating parser took {sw.Time()}");

        sw.Restart();
        var result = parser.ParsePartialString(BasicSample);
        sw.Stop();
        Console.WriteLine($"Parsing took {sw.Time()}");

        Console.WriteLine("\r\n==[ Failures ]===============================================================================");

        foreach (var fail in result.Scanner.ListFailures())
        {
            Console.WriteLine(fail);
        }

        Console.WriteLine("\r\n=================================================================================");

        Console.WriteLine("[["+result.Value+"]]");

        Console.WriteLine("\r\n=================================================================================");

        foreach (var blm in result.TaggedTokensDepthFirst())
        {
            Console.WriteLine($"{blm.Value.Trim()} [{blm.Tag}]");
        }

        Assert.That(result.Success, Is.True);
        Assert.That(result.Value, Is.EqualTo(BasicSample));
    }
}
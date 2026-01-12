using System.Diagnostics;
using System.Text;
using Gool.Results;
using NUnit.Framework;
using Samples;

namespace TestsStd;

[TestFixture]
public class PrefixCsvTests
{
    private const string SimpleSample =
        """

        [5] a,
            [3] b1, b2, b3
            [4] c1, c2, c3, c4
            d, e
        [3] f, g, h

        """;



    [Test]
    public void decompose_expression_to_tree()
    {
        var sw = new Stopwatch();
        sw.Start();
        var result = PrefixCsvExample.Parser.ParseEntireString(SimpleSample);
        sw.Stop();
        Console.WriteLine($"Parsing took {sw.Elapsed.TotalMicroseconds} µs");

        /*foreach (var fail in result.Scanner.ListFailures())
        {
            Console.WriteLine(fail);
        }*/

        // TODO: structure this better into tree, present properly.
        Assert.That(result.Success, Is.True, result + ": " + result.Value);

        var tokens = result.DepthFirstWalk();//TaggedTokensDepthFirst();
        foreach (var token in tokens)
        {
            Console.Write($"({token.Tag}: {token.Value}); ");
        }
    }


}
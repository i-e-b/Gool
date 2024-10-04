using System.Diagnostics;
using NUnit.Framework;
using Samples;
using TestsStd.Helpers;

namespace TestsStd;

[TestFixture]
public class CronTests
{
    [Test] // See https://crontab.guru/examples.html
    [TestCase("0 0 1 1 *")]
    [TestCase("0 0 1 * *")]
    [TestCase("0 0 * * 0")]
    [TestCase("0 0 * * *")]
    [TestCase("0 * * * *")]
    [TestCase("@midnight")]
    [TestCase("*/5 1,2,3 * * * UTC")]
    [TestCase("*/5 1-10/2 *** UTC")]
    [TestCase("*/5 1-10/2 * * mon-thu,SAT UTC")]
    [TestCase("*/5 1-10/2 * * 5L UTC")]
    [TestCase("45 23 * * 6")]
    [TestCase("? ? * * * *")]
    [TestCase("2 4 * * * Asia/Shanghai")]
    [TestCase("2 4 * * * 2017-2028 UTC")]
    [TestCase("0 0 1 */3 *")]
    [TestCase("0 9-17 * * *")]
    public void valid_expressions(string expr)
    {
        var parser = CronParser.Build();
        var sw     = new Stopwatch();

        sw.Start();
        var result = parser.ParseEntireString(expr);
        sw.Stop();

        Console.WriteLine($"Parsing took {sw.Time()}; Per character = {sw.Time(expr.Length)}");
        Assert.That(result.Success, Is.True);

        foreach (var part in result.TaggedTokensDepthFirst())
        {
            Console.WriteLine(part.Tag + ": " + part.Value);
        }
    }
}
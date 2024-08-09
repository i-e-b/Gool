using System.Diagnostics;
using NUnit.Framework;
using Phantom.Scanners;
using Samples;

namespace TestsStd;

[TestFixture]
public class MathTests
{
    [Test]
    [TestCase("6.5 + 3 * 2 - 5.5", 7)]
    [TestCase("(6.5 + 3) * (2 - 5.5)", -33.25)]
    public void scanning_expression(string expression, double expected)
    {
        var parser = MathParser.TheParser();
        var scanner = new ScanStrings(expression) { SkipWhitespace = true };

        var sw = new Stopwatch();
        sw.Start();
        var result = parser.Parse(scanner);
        sw.Stop();
        Console.WriteLine($"Parsing took {sw.Elapsed.TotalMicroseconds} µs");

        Console.WriteLine("\r\n=================================================================================");

        var input = new Stack<string>();
        foreach (var item in result.BottomLevelMatchesBreadthFirst())
        {
            if (item.Value == "(" || item.Value == ")") continue;
            input.Push(item.Value);
        }

        var values = EvaluateRpn(input);

        var final = values.Pop();
        Console.WriteLine($"Result = {final}");
        Assert.That(final, Is.EqualTo(expected));
    }

    private static Stack<double> EvaluateRpn(Stack<string> input)
    {
        var values = new Stack<double>();
        while (input.TryPop(out var item))
        {
            Console.WriteLine(item);
            
            switch (item)
            {
                case "+":
                {
                    var b = values.Pop();
                    var a = values.Pop();
                    values.Push(a + b);
                    break;
                }
                case "-":
                {
                    var b = values.Pop();
                    var a = values.Pop();
                    values.Push(a - b);
                    break;
                }
                case "*":
                {
                    var b = values.Pop();
                    var a = values.Pop();
                    values.Push(a * b);
                    break;
                }
                case "/":
                {
                    var b = values.Pop();
                    var a = values.Pop();
                    values.Push(a / b);
                    break;
                }

                default:
                    values.Push(double.Parse(item));
                    break;
            }
        }

        return values;
    }
}
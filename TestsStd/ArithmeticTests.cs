using System.Diagnostics;
using System.Globalization;
using Gool.Results;
using NUnit.Framework;
using Samples;

namespace TestsStd;

[TestFixture]
public class ArithmeticTests
{
    [Test]
    public void timing_test()
    {
        var parser = ArithmeticExample.Parser;
        var expression = "2^(1+3) * 3 * -2 - 5.5";

        var sw = new Stopwatch();
        sw.Start();
        for (int i = 0; i < 100; i++)
        {
            var result = parser.ParseEntireString(expression);
            if (!result.Success) Assert.Fail("Did not parse");
        }

        sw.Stop();
        Console.WriteLine($"Parsing took {sw.Elapsed.TotalMicroseconds / 100:0.0} µs on average");
    }

    [Test]
    [TestCase("6.5 + 3 * 2 - 5.5", 7)]
    [TestCase("-6.5 + 3 * -2 - 5.5", -18)]
    [TestCase("(6.5 + 3) * (2 - 5.5)", -33.25)]
    [TestCase("(6.5 + 3) * (5.5 - -2)", 71.25)]
    [TestCase("(6.5 + 3) * (5.5 - -0.2e1)", 71.25)]
    [TestCase("2^(1+3)", 16)]
    [TestCase("-2.71828182", -2.71828182)]
    [TestCase("+2.718e-5", 2.718E-05)]
    [TestCase("(+6.5 + +3) * (+5.5 - -2)", 71.25)]
    [TestCase("(+6.5++3)*(+5.5--2)", 71.25)] // If you ever do this, you're not my friend anymore.
    public void scanning_expression(string expression, double expected)
    {
        var sw = new Stopwatch();
        sw.Start();
        var result = ArithmeticExample.Parser.ParseEntireString(expression);
        sw.Stop();
        Console.WriteLine($"Parsing took {sw.Elapsed.TotalMicroseconds} µs");

        Console.WriteLine("\r\n=================================================================================");
        
        sw.Restart();
        // Get a tree from the matches
        var tree = TreeNode.FromParserMatch(result, prune: true);
        PrintRecursive(tree, 0);
        
        Console.WriteLine("\r\n=================================================================================");

        // Try to reduce to a single value
        var final = TreeNode.TransformTree(tree, ApplyOperation);
        PrintRecursive(final, 0);

        Assert.That(final?.Children.Count, Is.Zero, "Should have a final result");

        var finalValue = double.Parse(final?.Source.Value ?? "NaN");
        Assert.That(finalValue, Is.EqualTo(expected));
        sw.Stop();
        Console.WriteLine($"Tree operations and evaluation took {sw.Elapsed.TotalMicroseconds} µs");
    }

    [Test]
    [TestCase("6.5 + 3 * * 2 - 5.5")]
    [TestCase("2^()")]
    [TestCase("1 + ")]
    [TestCase("(+6.5+++3)*(+5.5---2)")]
    public void failure_cases(string expression)
    {
        var sw = new Stopwatch();
        sw.Start();
        var result = ArithmeticExample.Parser.ParseEntireString(expression);
        sw.Stop();
        Console.WriteLine($"Parsing took {sw.Elapsed.TotalMicroseconds} µs");

        Console.WriteLine(result.Description());

        Assert.That(result.Success, Is.False, "Invalid expression should result in failure");
    }

    private static TreeNode? ApplyOperation(TreeNode node)
    {
        if (node.Source.Tag is null)
        {
            if (node.Children.Count > 0) return node.Children[0]; // pull child up through joining nodes
            return null;
        }

        if (node.Source.Tag != ArithmeticExample.Operation) return node; // only look at operation nodes
        var operation = node.Source.Value;

        if (node.Children.Count < 2) throw new Exception("Invalid expression");
        var left = node.Children[0].Source;
        var right = node.Children[1].Source;

        if (!double.TryParse(left.Value, out var a) || !double.TryParse(right.Value, out var b)) return node; // one of our children is not a number

        // Both children are values: perform the operation
        var result = operation switch
        {
            "+" => a + b,
            "-" => a - b,
            "*" => a * b,
            "/" => a / b,
            "^" => Math.Pow(a, b),
            _ => throw new NotImplementedException($"Operation not implemented: '{operation}'")
        };

        // Return a new node with the calculated value
        return TreeNode.FromString(result.ToString(CultureInfo.InvariantCulture), ArithmeticExample.Value);
    }


    private static void PrintRecursive(TreeNode? node, int indent)
    {
        if (node is null) return;
        
        if (node.Source.Tag is not null) Console.WriteLine($"{I(indent)}{node.Source.Value} [{node.Source.Tag}] from {node.Source.SourceParser?.GetType().Name}");
        else Console.WriteLine($"{I(indent)}...");

        foreach (var childNode in node.Children)
        {
            PrintRecursive(childNode, indent+1);
        }
    }

    private static string I(int indent)
    {
        return new string(' ', indent * 2);
    }
}
using System.Diagnostics;
using System.Globalization;
using System.Text;
using Gool.Results;
using NUnit.Framework;
using Samples;
using TestsStd.Helpers;

namespace TestsStd;

[TestFixture]
public class ArithmeticTests
{
    [Test]
    public void timing_test()
    {
        var parser = ArithmeticExample.Arithmetic();
        var expression = "2^(1+3) * 3 * -2 - 5.5";

        var tryCount = 1000;
        var sw       = new Stopwatch();
        sw.Start();
        for (int i = 0; i < tryCount; i++)
        {
            var result = parser.ParseEntireString(expression);
            if (!result.Success) Assert.Fail("Did not parse");
        }

        sw.Stop();
        Console.WriteLine($"Parsing took {sw.Time(tryCount)}. Per character: {sw.Time(tryCount * expression.Length)}");
    }

    [Test]
    [TestCase("6.5 + 3 * 2 - 5.5", 7)]
    [TestCase("-6.5 + 3 * -2 - 5.5", -18)]
    [TestCase("(6.5 + 3) * (2 - 5.5)", -33.25)]
    [TestCase("(6.5 + 3) * (5.5 - -2)", 71.25)]
    [TestCase("(6.5 + 3) * (5.5 - -0.2e1)", 71.25)]
    [TestCase("2^(1+3)", 16)]
    [TestCase("33 / 3 - 11", 0)]
    [TestCase("11 - 33 / 3", 0)]
    [TestCase("-2.71828182", -2.71828182)]
    [TestCase("+2.718e-5", 2.718E-05)]
    [TestCase("(+6.5 + +3) * (+5.5 - -2)", 71.25)]
    [TestCase("((((6.5) + 3) * 2) - 5.5)", 13.5)]
    [TestCase("(+6.5++3)*(+5.5--2)", 71.25)] // If you ever do this, you're not my friend anymore.
    public void scanning_expression(string expression, double expected)
    {
        var sw = new Stopwatch();
        var parser = ArithmeticExample.Arithmetic();

        sw.Start();
        var result = parser.ParseEntireString(expression);
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
        var parser = ArithmeticExample.Arithmetic();

        var sw = new Stopwatch();
        sw.Start();
        var result = parser.ParseEntireString(expression);
        sw.Stop();
        Console.WriteLine($"Parsing took {sw.Elapsed.TotalMicroseconds} µs");

        Console.WriteLine(result.Description());

        Assert.That(result.Success, Is.False, "Invalid expression should result in failure");
    }

    [Test]
    public void partial_evaluation_of_expression()
    {
        var sw     = new Stopwatch();
        var parser = ArithmeticExample.ExpressionWithVariablesAndFunctions();

        sw.Start();
        var result = parser.ParseEntireString("2 * 4 * pi + cos((1+2) * phi) + random()");
        sw.Stop();
        Console.WriteLine($"Parsing took {sw.Elapsed.TotalMicroseconds} µs");

        var expectedPr = parser.ParseEntireString("8 * pi + cos(3 * phi) + random()");
        var expectedTn = TreeNode.FromParserMatch(expectedPr, prune: true);

        Console.WriteLine("\r\n=================================================================================");

        if (!result.Success) Console.WriteLine(string.Join("\r\n", result.Scanner.ListFailures()));
        Console.WriteLine(result.Value);
        Assert.That(result.Success, Is.True);

        foreach (var token in result.TaggedTokensDepthFirst())
        {
            Console.Write(token.Value + " " + token.Tag + ", ");
        }

        Console.WriteLine("\r\n=================================================================================");

        // Get a tree from the matches
        var tree = TreeNode.FromParserMatch(result, prune: true);
        PrintRecursive(tree, 0);

        Console.WriteLine("\r\n=================================================================================");

        // Try to reduce to a single value
        var final = TreeNode.TransformTree(tree, ApplyOperation);
        var sb1   = new StringBuilder();
        PrintRecursive(final, 0, sb1);
        Console.Write(sb1.ToString());

        Console.WriteLine("\r\n=================================================================================");

        var sb2 = new StringBuilder();
        PrintRecursive(expectedTn, 0, sb2);
        Console.Write(sb2.ToString());

        Assert.That(sb1.ToString(), Is.EqualTo(sb2.ToString()));
    }

    private static TreeNode? ApplyOperation(TreeNode node)
    {
        if (node.Source.Tag is null)
        {
            if (node.Children.Count > 1) return node;
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
        var sb = new StringBuilder();
        PrintRecursive(node, indent, sb);
        Console.Write(sb.ToString());
    }

    private static void PrintRecursive(TreeNode? node, int indent, StringBuilder sb)
    {
        if (node is null) return;

        var nextIndent = indent;
        if (node.Source.Tag is not null)
        {
            nextIndent++;
            sb.AppendLine($"{I(indent)}{node.Source.Value} [{node.Source.Tag}]");// from {node.Source.SourceParser.GetType().Name}");
        }

        foreach (var childNode in node.Children)
        {
            PrintRecursive(childNode, nextIndent, sb);
        }
    }

    private static string I(int indent)
    {
        return new string(' ', indent * 2);
    }
}
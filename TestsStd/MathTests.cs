using System.Diagnostics;
using System.Globalization;
using NUnit.Framework;
using Phantom.Results;
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
        
        sw.Restart();
        // Get a tree from the matches
        var tree = TreeNode.FromParserMatch(result, true);
        PrintRecursive(tree, 0);
        
        Console.WriteLine("\r\n=================================================================================");

        // Try to reduce to a single value
        var final = TreeNode.TransformTree(tree, ApplyOperation);
        PrintRecursive(final, 0);

        Assert.That(final?.Children.Count, Is.Zero, "Should have a final result");

        var finalValue = double.Parse(final?.Source.Value ?? "NaN");
        Assert.That(finalValue, Is.EqualTo(expected));
        sw.Stop();
        Console.WriteLine($"Evaluation took {sw.Elapsed.TotalMicroseconds} µs");
    }

    private static TreeNode? ApplyOperation(TreeNode node)
    {
        if (node.Source.Tag is null) // might be a joining tag.
        {
            if (node.Children.Count != 1) return node; // no changes
            return node.Children[0]; // pull child up
        }

        if (node.Source.Tag != MathParser.Operation) return node; // no changes
        var operation = node.Source.Value;

        if (node.Children.Count != 2) throw new Exception($"Expected 2 operands, got {node.Children.Count}");

        var left = node.Children[0].Source;
        var right = node.Children[1].Source;
        
        if (left.Tag != MathParser.Value) return node; // no changes
        if (right.Tag != MathParser.Value) return node; // no changes

        var a = double.Parse(left.Value);
        var b = double.Parse(right.Value);
        double result;
        
        // Both children are values, and we are an operation.
        // Perform the operation
        switch (operation)
        {
            case "+":
                result = a + b;
                break;
            
            case "-":
                result = a - b;
                break;
            
            case "*":
                result = a * b;
                break;
            
            case "/":
                result = a / b;
                break;
            
            default: throw new NotImplementedException($"Operation not implemented: '{operation}'");
        }
        
        // Return the new node
        var value = result.ToString(CultureInfo.InvariantCulture);
        return TreeNode.FromParserMatch(new ParserMatch(value, MathParser.Value), false);
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
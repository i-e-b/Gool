using System.Diagnostics;
using NUnit.Framework;
using Phantom.Results;
using Phantom.Scanners;
using Samples;
using SkinnyJson;

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

        //var tree = ScopeNode.RootNode();
        var tree = TreeNode.FromParserMatch(result);
        Console.WriteLine(Json.Beautify(Json.Freeze(tree)));
        //PrintRecursive(tree, 0);
        
        Console.WriteLine("\r\n=================================================================================");

        var input = new Stack<string>();
        foreach (var item in result.BottomLevelMatchesDepthFirst())
        {
            Console.WriteLine(item.Value);
            //if (item.Value == "(" || item.Value == ")") continue;
            input.Push(item.Value);
        }

        Console.WriteLine("\r\n=================================================================================");
        
        var values = EvaluateRpn(input);

        var final = values.Pop();
        Console.WriteLine($"Result = {final}");
        Assert.That(final, Is.EqualTo(expected));
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
                Console.WriteLine($"{I(indent)}{node.DataMatch?.Value} [{node.DataMatch?.Tag}]");
                break;
            case ScopeNodeType.ScopeChange:
                Console.WriteLine($"{I(indent + 1)}{node.OpeningMatch?.Value}");
                break;

            default:
                Assert.Fail($"Node does not have a valid type: {node}");
                break;
        }

        foreach (var childNode in node.Children)
        {
            PrintRecursive(childNode, indent + 2);
        }
    }

    private static string I(int indent)
    {
        return new string(' ', indent * 2);
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
                
                
                case "(":
                case ")":
                    break;

                default:
                    values.Push(double.Parse(item));
                    break;
            }
        }

        return values;
    }
}
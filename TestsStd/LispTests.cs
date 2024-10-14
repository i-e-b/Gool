using System.Diagnostics;
using Gool.Results;
using NUnit.Framework;
using Samples;

// ReSharper disable InconsistentNaming

namespace TestsStd;

[TestFixture]
public class LispTests
{
    private const string SimpleSample =
        """

        (loop for x in '(1 2 3)
          do (                  ; This is an example
            print "value" x
          )
        )

        """;
    
    private const string DeepSample =
        """

        (1
            (1_1 (1_1_1 1_1_2 1_1_3))
            (1_2 (1_2_1 1_2_2 1_1_3))
            (1_3 (1_3_1 1_3_2 1_3_3))
        )
        (2
            (2_1 (2_1_1 2_1_2 2_1_3))
            (2_2 (2_2_1 2_2_2 2_1_3))
            (2_3 (2_3_1 2_3_2 2_3_3))
        )
        (3
            (3_1 (3_1_1 3_1_2 3_1_3))
            (3_2 (3_2_1 3_2_2 3_1_3))
            (3_3 (3_3_1 3_3_2 3_3_3))
        )

        """;

    [Test]
    public void parse_s_expression()
    {
        Console.WriteLine(SimpleSample);
        
        Console.WriteLine("=================================================================================");

        var result = LispExample.Parser.ParsePartialString(SimpleSample);

        foreach (var match in result.BottomLevelMatchesDepthFirst())
        {
            Console.Write(match.Value);
            Console.Write(" ");
        }

        Console.WriteLine("\r\n=================================================================================");
        
        foreach (var fail in result.Scanner.ListFailures())
        {
            Console.WriteLine(fail);
        }

        Assert.That(result.Success, Is.True, result + ": " + result.Value);
        Assert.That(result.Value.Trim(), Is.EqualTo(SimpleSample.Trim()));

        var taggedTokens = result.TaggedTokensDepthFirst();

        var indent = 0;
        foreach (var token in taggedTokens)
        {
            switch (token.Tag)
            {
                case LispExample.Atom:
                    Console.WriteLine(I(indent) + token.Tag + " " + token.Value);
                    break;
                
                case LispExample.String:
                    Console.WriteLine(I(indent) + token.Tag + " " + token.Value);
                    break;
                
                case LispExample.Number:
                    Console.WriteLine(I(indent) + token.Tag + " " + token.Value);
                    break;

                case LispExample.Name:
                    Console.WriteLine(I(indent) + token.Value);
                    break;

                case LispExample.Quote:
                    Console.WriteLine(I(indent) + "'(");
                    indent++;
                    break;

                case LispExample.List:
                    Console.WriteLine(I(indent) + "(");
                    indent++;
                    break;

                case LispExample.End:
                    indent--;
                    Console.WriteLine(I(indent) + ")");
                    break;
            }
        }
    }

    [Test]
    public void decompose_s_expression_to_tree()
    {
        var sw = new Stopwatch();
        sw.Start();
        var result = LispExample.Parser.ParsePartialString(SimpleSample);
        sw.Stop();
        Console.WriteLine($"Parsing took {sw.Elapsed.TotalMicroseconds} µs");
        
        Assert.That(result.Success, Is.True, result + ": " + result.Value);
        
        var scopes = ScopeNode.FromMatch(result);

        PrintRecursive(scopes, 0);
    }

    [Test]
    public void scanning_tree_form()
    {
        var sw = new Stopwatch();
        sw.Start();
        var result = LispExample.Parser.ParsePartialString(DeepSample);
        sw.Stop();
        Console.WriteLine($"Parsing took {sw.Elapsed.TotalMicroseconds} µs");

        Console.WriteLine("\r\n== Depth first ===============================================================================");
        var tree = ScopeNode.FromMatch(result);
        
        tree.DepthFirstWalk(n =>
        {
            if (n.NodeType == ScopeNodeType.Data) Console.WriteLine(n.ToString());
        });

        Console.WriteLine("\r\n== Breadth first ===============================================================================");
        
        tree.BreadthFirstWalk(n =>
        {
            if (n.NodeType == ScopeNodeType.Data) Console.WriteLine(n.ToString());
        });
    }


    private static void PrintRecursive<T>(ScopeNode<T> node, int indent)
    {
        switch (node.NodeType)
        {
            case ScopeNodeType.Root:
                Console.WriteLine("Document");
                if (node.OpeningMatch is not null || node.ClosingMatch is not null) Console.WriteLine("Unbalanced scopes!");
                break;
            case ScopeNodeType.Data:
                Console.WriteLine(I(indent) + node.DataMatch?.Value);
                break;
            case ScopeNodeType.ScopeChange:
                switch (node.OpeningMatch?.Tag)
                {
                    case LispExample.Quote:
                        Console.WriteLine(I(indent) + "Quoted list:");
                        break;
                    case LispExample.List:
                        Console.WriteLine(I(indent) + "Expression list:");
                        break;
                    default:
                        throw new ArgumentOutOfRangeException();
                }

                break;
            default:
                throw new ArgumentOutOfRangeException();
        }

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
using System.Diagnostics;
using NUnit.Framework;
using Phantom;
using Phantom.Results;
using Phantom.Scanners;

// ReSharper disable InconsistentNaming

namespace TestsStd;

[TestFixture]
public class LispTests
{
    private const string Sample =
        """

        (loop for x in '(1 2 3)
          do (print "value" x))

        """;

    private IParser MakeParser()
    {
        BNF identifier = "#[_a-zA-Z][_a-zA-Z0-9]*";
        BNF number = "#[0-9][_a-zA-Z0-9]*";

        BNF atom = ':' > identifier;
        BNF quoted_string = '"' > identifier > '"'; // this is wrong, but good enough for this test
        BNF normal_list = '(';
        BNF quoted_list = "'(";
        BNF end_list = ')';
        
        BNF list_item = identifier.Tagged("Name") | atom | quoted_string | number;
        BNF start_list = normal_list | quoted_list;

        quoted_list.Tag("Quote");
        atom.Tag("Atom");
        quoted_string.Tag("String");
        number.Tag("Number");
        normal_list.Tag("List");
        end_list.Tag("End");

        normal_list.OpenScope();
        quoted_list.OpenScope();
        end_list.CloseScope();

        return BNF.Recursive(tree => +(list_item | start_list | end_list | tree)).Result();
    }

    [Test]
    public void parse_s_expression()
    {
        Console.WriteLine(Sample);
        
        Console.WriteLine("=================================================================================");

        var parser = MakeParser();
        var scanner = new ScanStrings(Sample) { SkipWhitespace = true };

        var result = parser.Parse(scanner);

        foreach (var match in result.BottomLevelMatches())
        {
            Console.Write(match.Value);
            Console.Write(" ");
        }

        Console.WriteLine("\r\n=================================================================================");
        
        foreach (var fail in scanner.ListFailures())
        {
            Console.WriteLine(fail);
        }

        Assert.That(result.Success, Is.True, result + ": " + result.Value);
        Assert.That(result.Value.Trim(), Is.EqualTo(Sample.Trim()));

        var taggedTokens = result.TaggedTokens();

        var indent = 0;
        foreach (var token in taggedTokens)
        {
            switch (token.Tag)
            {
                case "Atom":
                    Console.WriteLine(I(indent) + "Atom " + token.Value);
                    break;
                
                case "String":
                    Console.WriteLine(I(indent) + "String " + token.Value);
                    break;
                
                case "Number":
                    Console.WriteLine(I(indent) + "Number " + token.Value);
                    break;

                case "Name":
                    Console.WriteLine(I(indent) + token.Value);
                    break;

                case "Quote":
                    Console.WriteLine(I(indent) + "'(");
                    indent++;
                    break;

                case "List":
                    Console.WriteLine(I(indent) + "(");
                    indent++;
                    break;

                case "End":
                    indent--;
                    Console.WriteLine(I(indent) + ")");
                    break;
            }
        }
    }

    [Test]
    public void decompose_s_expression_to_tree()
    {
        var parser = MakeParser();
        var scanner = new ScanStrings(Sample) { SkipWhitespace = true };

        var sw = new Stopwatch();
        sw.Start();
        var result = parser.Parse(scanner);
        sw.Stop();
        Console.WriteLine($"Parsing took {sw.Elapsed.TotalMicroseconds} µs");
        
        Assert.That(result.Success, Is.True, result + ": " + result.Value);
        
        var scopes = result.ToScopes();

        PrintRecursive(scopes, 0);
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
                Console.WriteLine(I(indent) + node.DataMatch?.Value);
                break;
            case ScopeNodeType.ScopeChange:
                switch (node.OpeningMatch?.Tag)
                {
                    case "Quote":
                        Console.WriteLine(I(indent) + "Quoted list:");
                        break;
                    case "List":
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
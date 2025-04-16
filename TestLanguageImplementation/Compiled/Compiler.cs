using System.Diagnostics;
using System.Globalization;
using System.Text;
using Gool.Results;
using TestLanguageImplementation.Helpers;
using TestLanguageImplementation.Interpreted;

namespace TestLanguageImplementation.Compiled;

public class Compiler
{
    private readonly Dictionary<string, CompiledFunction> _functionDefs        = new();
    private readonly Dictionary<string, string>           _fileHeaders         = new();
    private readonly Queue<CompiledFunction>              _referencedFunctions = new();

    public List<OpCode> Compile(string program)
    {
        // Parse the program
        var entryFunc = ParseProgram(program);
        _referencedFunctions.Enqueue(entryFunc);

        // We should now explore out from the entry point and only compile functions that are referenced
        while (_referencedFunctions.Count > 0)
        {
            CompileFunction(_referencedFunctions.Dequeue());
        }

        // Link the compiled parts together
        var output = new List<OpCode>();
        return output;
    }

    /// <summary>
    /// Try to compile a function,
    /// and push any functions that are referenced
    /// </summary>
    private void CompileFunction(CompiledFunction func)
    {
        if (func.OpCodes.Count > 0) return; // already compiled
        Console.WriteLine($"Start of function '{func.Name}'");

        // Otherwise, process next statement
        foreach (var loc in func.Source.Children)
        {
            switch (loc.Tag)
            {
                case LanguageDefinition.Assignment:
                    // Reduce expression, convert to stack-ops, then do assignment
                    Console.WriteLine("Assignment");
                    break;

                case LanguageDefinition.FunctionCall:
                    // Reduce expression, convert to stack-ops, issue call, add func to _referencedFunctions
                    Console.WriteLine("FunctionCall");
                    break;

                case LanguageDefinition.StartBlock:
                    // Increment scope for resolving
                    Console.WriteLine("StartBlock");
                    break;

                case LanguageDefinition.EndBlock:
                    // Decrement scope for resolving
                    Console.WriteLine("EndBlock");
                    break;

                case LanguageDefinition.IfBlock:
                    // Treat either side like a sub-function?
                    Console.WriteLine("IfBlock");
                    break;

                case LanguageDefinition.Loop:
                    // Treat body like a sub-function, insert loop op
                    Console.WriteLine("Loop");
                    break;

                case LanguageDefinition.Comment: break; // Ignore

                case LanguageDefinition.ReturnCall:
                    // Expr, stack ops, return op
                    Console.WriteLine("ReturnCall");
                    break;

                case LanguageDefinition.BreakCall:
                    // Exit loop
                    Console.WriteLine("BreakCall");
                    break;

                default:
                    throw new Exception($"Unexpected tag at Location={loc.AnyMatch?.Offset}; Tag='{loc.Tag ?? "<null>"}';");
            }
        }

        // Step through, spit out op codes
        throw new NotImplementedException("Need to implement 'CompileFunction'");
    }

    /// <summary>
    /// Read the code file, and build basic scope-node stuff.
    /// Returns the entry point.
    /// </summary>
    private CompiledFunction ParseProgram(string program)
    {
        var sw     = Stopwatch.StartNew();
        var parser = LanguageDefinition.Instance;
        sw.Stop();
        Console.WriteLine($"    Creating parser: {sw.Time()}");

        sw.Restart();
        var result = parser.ParseEntireString(program, diagnostics: false);
        if (!result.Success)
        {
            result = parser.ParseEntireString(program, diagnostics: true);
            throw new Exception("Program is invalid.\r\n" + string.Join("\r\n", result.Scanner.ListFailures()));
        }

        sw.Stop();
        Console.WriteLine($"    Parse input: {sw.Time()}");

        sw.Restart();
        var scopeTree = ScopeNode.FromMatch(result);
        sw.Stop();
        Console.WriteLine($"    Build scope: {sw.Time()}");

        // Read header, check entry point and version
        sw.Restart();
        foreach (var header in result.FindByTag(LanguageDefinition.FileHeaderSetting))
        {
            _fileHeaders.Add(
                header.GetTag(LanguageDefinition.FileHeaderKey)!.Value,
                header.GetTag(LanguageDefinition.FileHeaderValue)!.Value
            );
        }

        if (!_fileHeaders.TryGetValue("version", out var version) || version != "1")
        {
            throw new Exception("Expected version 1 file, but was not found");
        }

        // Read function entry points, and store them
        foreach (var scope in scopeTree.Children)
        {
            if (scope.Tag != LanguageDefinition.FunctionDefinition) continue;
            var name = scope.FirstByTag(LanguageDefinition.FunctionName)?.Value;

            if (name is null) throw new Exception("Unexpected empty name in function def");
            if (_functionDefs.ContainsKey(name)) throw new Exception($"Duplicate definition of function '{name}'");

            var block = scope.FirstByTag(LanguageDefinition.StartBlock);
            if (block is null) throw new Exception("Unexpected empty block in function def");

            _functionDefs.Add(name, new CompiledFunction { Name = name, Source = block });
            Console.WriteLine($"'{name}' at {block.AnyMatch?.Offset}:{block.AnyMatch?.Length}");
        }

        if (!_fileHeaders.TryGetValue("entry", out var entryFuncName)) throw new Exception("No entry point defined");
        if (!_functionDefs.TryGetValue(entryFuncName, out var entryFunc)) throw new Exception($"Did not find definition of entry function '{entryFuncName}'");

        return entryFunc;
    }

    /// <summary>
    /// Convert string with outer quotes removed, and escapes resolved
    /// </summary>
    private static Value UnpackQuotedString(TreeNode<Value> node)
    {
        var src = node.Source.Value;
        var dst = new StringBuilder();

        for (var i = 1; i < src.Length - 1; i++)
        {
            var c = src[i];
            if (c == '\\')
            {
                i++;
                var t = src[i];
                switch (t)
                {
                    case 'u':
                    {
                        dst.Append(
                            char.ConvertFromUtf32(int.Parse("" + src[i + 1] + src[i + 2] + src[i + 3] + src[i + 4],
                                NumberStyles.HexNumber)));
                        i += 4;
                        break;
                    }

                    case '\\':
                        dst.Append('\\');
                        break;
                    case '"':
                        dst.Append('"');
                        break;
                    case '/':
                        dst.Append('/');
                        break;
                    case 'b':
                        dst.Append('\b');
                        break;
                    case 'f':
                        dst.Append('\f');
                        break;
                    case 'n':
                        dst.Append('\n');
                        break;
                    case 'r':
                        dst.Append('\r');
                        break;
                    case 't':
                        dst.Append('\t');
                        break;
                }
            }
            else dst.Append(c);
        }

        var value = new Value(dst.ToString());
        node.UserData = value;
        return value;
    }

    private static string ShortenString(string? str)
    {
        if (str is null) return "";
        if (str.Length < 25) return str;
        return str[..25];
    }
}

public class CompiledFunction
{
    public string Name { get; set; } = "";
    public ScopeNode<None> Source { get; set; } = new();
    public List<OpCode> OpCodes { get; } = new();
}

public class OpCode
{
}
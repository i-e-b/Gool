using System.Text;
using Gool.Results;
using SkinnyJson;

namespace TestLanguageImplementation;

/// <summary>
/// Language interpreter
/// </summary>
public class Interpreter
{
    private readonly Dictionary<string, ScopeNode> _functionDefs = new();
    private readonly Dictionary<string, string>    _fileHeaders  = new();
    private readonly Stack<ScopeNode>              _returnStack  = new();
    private readonly Stack<VarScope>               _scopeStack   = new();
    private readonly Queue<string>                 _userInput    = new();
    private readonly StringBuilder                 _output       = new();
    private readonly ScopeNode?                    _programPointer;

    public Interpreter(string program)
    {
        // Parse the program
        var parser = LanguageDefinition.Instance;
        var result = parser.ParseEntireString(program);
        if (!result.Success) throw new Exception("Program is invalid.\r\n" + string.Join("\r\n", result.Scanner.ListFailures()));

        var scopeTree = ScopeNode.FromMatch(result);
        PrintRecursive(scopeTree, 0);

        // Read header, check entry point and version
        Console.WriteLine("=[ Read headers ]=================================================");
        foreach (var header in result.FindByTag(LanguageDefinition.FileHeaderSetting))
        {
            _fileHeaders.Add(
                header.GetTag(LanguageDefinition.FileHeaderKey)!.Value,
                header.GetTag(LanguageDefinition.FileHeaderValue)!.Value
            );
        }

        Console.WriteLine(Json.Freeze(_fileHeaders));

        if (!_fileHeaders.TryGetValue("version", out var version) || version != "1")
        {
            throw new Exception("Expected version 1 file, but was not found");
        }

        // Read function entry points, and store them
        Console.WriteLine("=[ Mark Function Entry ]==========================================");
        foreach (var scope in scopeTree.Children)
        {
            if (scope.Tag != LanguageDefinition.FunctionDefinition) continue;
            var name = scope.FirstByTag(LanguageDefinition.FunctionName)?.Value;

            if (name is null) throw new Exception("Unexpected empty name in function def");
            if (_functionDefs.ContainsKey(name)) throw new Exception($"Duplicate definition of function '{name}'");

            var block = scope.FirstByTag(LanguageDefinition.StartBlock);
            if (block is null) throw new Exception("Unexpected empty block in function def");

            _functionDefs.Add(name, block);
            Console.WriteLine($"'{name}' at {block.AnyMatch?.Offset}:{block.AnyMatch?.Length}");
        }

        if (!_fileHeaders.TryGetValue("entry", out var entryFuncName))
            throw new Exception("No entry point defined");
        if (!_functionDefs.TryGetValue(entryFuncName, out var entryFunc))
            throw new Exception($"Did not find definition of entry function '{entryFuncName}'");

        _returnStack.Push(entryFunc);
        _programPointer = entryFunc.Children.FirstOrDefault();
        _scopeStack.Push(new VarScope());

        Console.WriteLine($"Should start at {entryFunc}, {_programPointer?.Value ?? "<null>"}");
    }

    /// <summary>
    /// Call step repeatedly until it returns false.
    /// </summary>
    public bool Step()
    {
        if (_programPointer is null) return false;

        switch (_programPointer.Tag)
        {
            case LanguageDefinition.Assignment:
                AssignInScope();
                break;

            default:
                Console.WriteLine($"Unexpected tag at program counter: '{_programPointer.Tag ?? "<null>"}'");
                return false;
        }

        // TODO: walk the scope tree, and for expressions, build a TreeNode and reduce it.
        // TODO: call stack and multiple `fn` defs.

        return false;
    }

    private void AssignInScope()
    {
        if (_programPointer is null) return;

        var source = _programPointer.FirstByTag(LanguageDefinition.Expression);
        var target = _programPointer.FirstByTag(LanguageDefinition.Variable);
        if (source is null) throw new Exception($"Invalid assignment at {_programPointer}");
        if (target is null) throw new Exception($"Invalid assignment at {_programPointer}");

        // TODO: try to resolve a value, otherwise deal with calls to build a value

        // Will need some way doing the 'Step()' thing, with potentially multiple calls to resolve.
        // 1. Build the expression TreeNode, attach it to our program pointer (probably need another data structure)
        // 2. Reduce the expression tree, switching to a new program pointer as required.
        // 3. When tree has a definite value, do the assignment
        // 4. If we have no value, and no reductions, throw exception.

        Console.WriteLine($"Assign '{target.Value}' with '{source.Value}'");
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
                Console.WriteLine(I(indent) + node.Value + " [" + node.Tag + "]");
                break;
            case ScopeNodeType.ScopeChange:
                Console.WriteLine(I(indent) + node.Value + " >[" + node.Tag + "]");
                break;
            default:
                throw new ArgumentOutOfRangeException();
        }

        foreach (var childNode in node.Children)
        {
            PrintRecursive(childNode, indent + 1);
        }
    }

    private static string I(int indent)
    {
        return new string(' ', indent * 2);
    }

    /// <summary>
    /// Simulate sending user input to the interpreter
    /// </summary>
    public void SendLine(string input)
    {
        _userInput.Enqueue(input);
    }

    /// <summary>
    /// Get the console output written so far, and clear it
    /// </summary>
    public string GetOutput()
    {
        var outp = _output.ToString();
        _output.Clear();
        return outp;
    }
}
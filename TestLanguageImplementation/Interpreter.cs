using System.Globalization;
using System.Text;
using Gool.Results;
using SkinnyJson;

namespace TestLanguageImplementation;

/// <summary>
/// State of execution, as stored in the interpreter return stack
/// </summary>
public class ProgramPtr
{
    /// <summary>
    /// Location in program
    /// </summary>
    public ScopeNode? Location;

    /// <summary>
    /// Expression that is being resolved, if any
    /// </summary>
    public TreeNode? Expression;

    /// <summary>
    /// Expression node to be filled with function's return value
    /// </summary>
    public TreeNode? ReturnValue;
}

/// <summary>
/// Language interpreter
/// </summary>
public class Interpreter
{
    private readonly Dictionary<string, ScopeNode> _functionDefs = new();
    private readonly Dictionary<string, string>    _fileHeaders  = new();
    private readonly Stack<ProgramPtr>             _returnStack  = new();
    private readonly Stack<VarScope>               _scopeStack   = new();
    private readonly Queue<string>                 _userInput    = new();
    private readonly StringBuilder                 _output       = new();

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

        _returnStack.Push(new ProgramPtr{Location = entryFunc.Children.FirstOrDefault()});
        _scopeStack.Push(new VarScope(null));

        Console.WriteLine($"Should start at {entryFunc}, {_returnStack.Peek().Location?.Value ?? "<null>"}");
    }

    /// <summary>
    /// Call step repeatedly until it returns false.
    /// </summary>
    public bool Step()
    {
        if (_returnStack.Count < 1) return false; // end of program
        var pc = _returnStack.Peek();
        if (pc.Location is null)
        {
            _returnStack.Pop();
            return true;
        }

        switch (pc.Location.Tag)
        {
            case LanguageDefinition.Assignment:
                return AssignInScope();

            case LanguageDefinition.FunctionCall:
                Console.WriteLine("not done: func");
                break;

            case LanguageDefinition.IfBlock:
                Console.WriteLine("not done: if/else");
                break;

            case LanguageDefinition.Loop:
                Console.WriteLine("not done: loop");
                break;

            default:
                Console.WriteLine($"Unexpected tag at program counter: '{pc.Location.Tag ?? "<null>"}'");
                return false;
        }

        // TODO: walk the scope tree, and for expressions, build a TreeNode and reduce it.
        // TODO: call stack and multiple `fn` defs.

        return false;
    }

    private bool AssignInScope()
    {
        var pc = _returnStack.Peek();
        if (pc.Location is null) return false;

        var source = pc.Location.FirstByTag(LanguageDefinition.Expression);
        var target = pc.Location.FirstByTag(LanguageDefinition.Variable);
        if (source?.AnyMatch is null) throw new Exception($"Invalid assignment at {pc.Location}");
        if (target is null) throw new Exception($"Invalid assignment at {pc.Location}");

        // TODO: try to resolve a value, otherwise deal with calls to build a value

        // Will need some way doing the 'Step()' thing, with potentially multiple calls to resolve.
        // 1. Build the expression TreeNode, attach it to our program pointer (probably need another data structure)
        // 2. Reduce the expression tree, switching to a new program pointer as required.
        // 3. When tree has a definite value, do the assignment
        // 4. If we have no value, and no reductions, throw exception.

        var tree  = TreeNode.FromParserMatch(source.AnyMatch, prune: true);
        var final = TreeNode.TransformTree(tree, ApplyOperation);

        if (final?.Children.Count == 0) // we have a single value or name
        {
            var isNumber = double.TryParse(final.Source.Value, out var num);
            if (isNumber)
            {
                SetScopeValue(name: target.Value, value: new Value(num));
                AdvanceProgramPointer();
                return true;
            }

            // a function call, string, or variable name
            throw new Exception($"Not implemented: {final.Source.Tag}");
        }

        // need to resolve variables, or call functions
        //var toResolve = final.DeepestFirst().FirstOrDefault();
        // TODO: need to store 'final' as the next _programPointer.
        pc.Expression = final;

        Console.WriteLine($"Assign '{target.Value}' with '{source.Value}'");
        throw new Exception("Not implemented");
    }

    private void AdvanceProgramPointer()
    {
        var pc = _returnStack.Peek();
        // Either move the pointer to its next sibling, or walk up stack and do the same

        while (pc.Location is not null)
        {
            var next = pc.Location.NextNode;
            if (next is not null)
            {
                Console.WriteLine($"Next -> {next.Value}");
                pc.Location = next;
                return;
            }

            pc.Location = pc.Location.Parent;
        }

        Console.WriteLine("End of call");
        _returnStack.Pop();
    }

    /// <summary>
    /// Try to change a value.
    /// If not found, create a new value in the current scope
    /// </summary>
    private void SetScopeValue(string name, Value value)
    {
        _scopeStack.Peek().Set(name, value);
    }


    private static TreeNode? ApplyOperation(TreeNode node)
    {
        if (node.Source.Tag is null)
        {
            if (node.Children.Count > 1) return node;
            if (node.Children.Count > 0) return node.Children[0]; // pull child up through joining nodes
            return null;
        }

        if (node.Source.Tag == LanguageDefinition.Expression)
        {
        }

        if (node.Source.Tag != LanguageDefinition.MathOp) return node; // only look at operation nodes
        var operation = node.Source.Value;

        if (node.Children.Count < 2) throw new Exception("Invalid expression");
        var left  = node.Children[0].Source;
        var right = node.Children[1].Source;

        // TODO: might have strings, or numeric values
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
        return TreeNode.FromString(result.ToString(CultureInfo.InvariantCulture), LanguageDefinition.Number);
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
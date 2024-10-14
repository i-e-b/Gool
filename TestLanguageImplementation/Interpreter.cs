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
    public ScopeNode<None>? Location;

    /// <summary>
    /// Expression that is being resolved, if any
    /// </summary>
    public TreeNode<Value>? Expression;

    /// <summary>
    /// Final value of the expression
    /// </summary>
    public Value? ReturnValue;
}

/// <summary>
/// Language interpreter
/// </summary>
public class Interpreter
{
    private readonly Dictionary<string, ScopeNode<None>> _functionDefs = new();
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

        // Are we working on an expression?
        if (pc.Expression is not null && pc.ReturnValue is null)
        {
            return ProcessExpression();
        }

        // Otherwise, process next statement
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

    private bool ProcessExpression()
    {

        Console.WriteLine("Not done: reduce expression to a value");
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

        // Are we done?
        if (pc.ReturnValue is not null)
        {
            Console.WriteLine($"Assign '{target.Value}' with '{pc.ReturnValue}'");
            SetScopeValue(target.Value, pc.ReturnValue);
            AdvanceProgramPointer();
            return true;
        }

        // Read the expression
        TreeNode<Value> exprTree;
        if (pc.Expression is not null)
        {
            exprTree = pc.Expression;
        }
        else
        {
            var tree = TreeNode<Value>.FromParserMatch(source.AnyMatch, prune: true);
            exprTree = TreeNode<Value>.TransformTree(tree, ReduceExpressionWithMath) ?? throw new Exception($"Invalid expression: '{source.Value}'");
            Console.WriteLine("===[ expression ]=======");
            PrintRecursive(exprTree, 0);
            Console.WriteLine("========================");
        }

        if (exprTree?.UserData is not null) // we have a single value or name
        {
            // Have we reduced to a single value?
            if (exprTree.UserData is not null)
            {
                SetScopeValue(name: target.Value, value: exprTree.UserData);
                AdvanceProgramPointer();
                return true;
            }

            // Otherwise it contains a function call. We need to push a return frame and continue into the call
        }

        // need to resolve variables, or call functions
        //var toResolve = final.DeepestFirst().FirstOrDefault();
        // TODO: need to store 'final' as the next _programPointer.
        pc.Expression = exprTree;

        Console.WriteLine($"Will need to resolve '{source.Value}' to assign '{target.Value}'");
        return true;
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

    /// <summary>
    /// Try to read a value.
    /// If not found, returns invalid type
    /// </summary>
    private Value GetScopeValue(string name)
    {
        return _scopeStack.Peek().Get(name);
    }


    private TreeNode<Value>? ReduceExpressionWithMath(TreeNode<Value> node)
    {
        if (node.UserData is not null) return node; // We've already handled this one

        if (node.Source.Tag is null)
        {
            if (node.Children.Count > 1) return node;
            if (node.Children.Count > 0) return node.Children[0]; // pull child up through joining nodes
            return null;
        }

        // Lift value out of expression if we have it
        if (node.Source.Tag == LanguageDefinition.Expression && node.Children.Count == 1) return node.Children[0];

        // TODO: for single nodes, try and resolve a value from either double.Parse, "string", or variable name.
        //     : then add this to the UserData.
        //     : For nodes that are operations, check if we have two nodes with UserData.
        //     : For nodes that are calls, check we have all children with UserData
        //     : node.UserData = ...

        if (double.TryParse(node.Source.Value, out var dbl))
        {
            node.UserData = new Value { NumericValue = dbl, Kind = ValueKind.Numeric };
            node.Children.Clear();
            return node;
        }

        // Try and replace this node with a variable value
        if (node.Source.Tag == LanguageDefinition.Variable)
        {
            var value = GetScopeValue(node.Source.Value);
            if (value.Kind == ValueKind.Invalid) throw new Exception($"No such variable: '{node.Source.Value}'");
            node.UserData = value;
            return node;
        }

        if (node.Source.Tag != LanguageDefinition.MathOp) return node; // only look at operation nodes
        var operation = node.Source.Value;

        if (node.Children.Count < 2) throw new Exception("Invalid expression");
        var left  = node.Children[0].UserData;
        var right = node.Children[1].UserData;

        // might have strings, numeric values, or nothing

        if (left is null || right is null) return node; // not resolved yet

        // Both children are values: perform the operation
        if (left.Kind == ValueKind.Numeric && right.Kind == ValueKind.Numeric)
        {
            var result = operation switch
            {
                "+" => left.NumericValue + right.NumericValue,
                "-" => left.NumericValue - right.NumericValue,
                "*" => left.NumericValue * right.NumericValue,
                "/" => left.NumericValue / right.NumericValue,
                "^" => Math.Pow(left.NumericValue, right.NumericValue),
                _ => throw new NotImplementedException($"Operation not implemented: '{operation}'")
            };
            node.UserData = new Value
            {
                Kind = ValueKind.Numeric, NumericValue = result
            };
            node.Children.Clear();
            return node;
        }
        else // one child is a string. Do string operations
        {
            var a = left.Kind == ValueKind.String ? left.StringValue : left.NumericValue.ToString(CultureInfo.InvariantCulture);
            var b = right.Kind == ValueKind.String ? right.StringValue : right.NumericValue.ToString(CultureInfo.InvariantCulture);
            var result = operation switch
            {
                "+" => a + b,
                _ => throw new NotImplementedException($"Operation not valid for strings: '{operation}'")
            };
            node.UserData = new Value
            {
                Kind = ValueKind.String, StringValue = result
            };
            node.Children.Clear();
            return node;
        }
    }

    private static void PrintRecursive(TreeNode<Value>? node, int indent)
    {
        if (node is null) return;

        var nextIndent = indent;
        if (node.Source.Tag is not null)
        {
            nextIndent++;
            Console.WriteLine($"{I(indent)}{node.Source.Value} [{node.Source.Tag}, {node.UserData?.ToString() ?? "<null>"}]");
        }
        else
        {
            Console.WriteLine($"...{node.Source.Value}");
        }

        foreach (var childNode in node.Children)
        {
            PrintRecursive(childNode, nextIndent);
        }
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
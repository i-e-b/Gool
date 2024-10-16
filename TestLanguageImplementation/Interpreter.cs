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
    /// Container for final value of the expression or function call
    /// </summary>
    public Value? ReturnValue;
}

/// <summary>
/// Language interpreter
/// </summary>
public class Interpreter
{
    private readonly Dictionary<string, ScopeNode<None>> _functionDefs = new();
    private readonly Dictionary<string, string>          _fileHeaders  = new();
    private readonly Stack<ProgramPtr>                   _returnStack  = new();
    private readonly Stack<VarScope>                     _scopeStack   = new();
    private readonly Queue<string>                       _userInput    = new();
    private readonly StringBuilder                       _output       = new();

    public Interpreter(string program)
    {
        // Parse the program
        var parser = LanguageDefinition.Instance;
        var result = parser.ParseEntireString(program);
        if (!result.Success) throw new Exception("Program is invalid.\r\n" + string.Join("\r\n", result.Scanner.ListFailures()));

        var scopeTree = ScopeNode.FromMatch(result);
        //PrintRecursive(scopeTree, 0);

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

        _returnStack.Push(new ProgramPtr { Location = entryFunc.Children.FirstOrDefault() });
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
        if (pc.Expression is not null && pc.ReturnValue is null && pc.Expression.UserData is null)
        {
            return ContinueExpression();
        }

        // Otherwise, process next statement
        switch (pc.Location.Tag)
        {
            case LanguageDefinition.Assignment:
                return AssignInScope();

            case LanguageDefinition.FunctionCall:
                return CallFunction();

            case LanguageDefinition.IfBlock:
                Console.WriteLine("not done: if/else");
                break;

            case LanguageDefinition.Loop:
                Console.WriteLine("not done: loop");
                break;

            case LanguageDefinition.Comment:
                AdvanceProgramPointer();
                return true;

            case LanguageDefinition.ReturnCall:
                Console.WriteLine("Need to do function return!");
                break;

            default:
                Console.WriteLine($"Unexpected tag at program counter: '{pc.Location.Tag ?? "<null>"}'");
                return false;
        }

        // TODO: walk the scope tree, and for expressions, build a TreeNode and reduce it.
        // TODO: call stack and multiple `fn` defs.

        return false;
    }

    /// <summary>
    /// Handle a statement that is a function call
    /// </summary>
    private bool CallFunction()
    {
        var pc = _returnStack.Peek();
        pc.ReturnValue = null;

        var func = pc.Location ?? throw new Exception($"Invalid function call at {pc.Location}");

        // Handle the entire call as if it was an expression we want the value from
        var done = HandleExpression(pc, func);
        if (done)
        {
            // Ignore any value that came back, move on.
            AdvanceProgramPointer();
        }

        return true;
    }

    /// <summary>
    /// Handle built-in functions (print, read_line)
    /// </summary>
    private bool HandleBuiltInFunction(string funcName, List<Value?> paramValues)
    {
        switch (funcName)
        {
            case "print":
            {
                Console.WriteLine($"===[ print ]=====> '{paramValues.FirstOrDefault()}'");
                _output.Append(paramValues.FirstOrDefault());
                return true;
            }
            case "read_line":
            {
                if (_userInput.Count < 1) throw new Exception("Not enough user input supplied");
                var input = _userInput.Dequeue();
                Console.WriteLine($"===[ read_line ]==> '{input}'");

                var pc = _returnStack.Peek();
                pc.ReturnValue = new Value(input);
                return true;
            }

            default: return false;
        }
    }

    /// <summary>
    /// Step through an in-progress expression
    /// </summary>
    private bool ContinueExpression()
    {
        var pc         = _returnStack.Peek();
        var actionable = pc.Expression!.FindBy(IsFunctionReadyToCall).ToList(); // this returns the function name node

        if (actionable.Count < 1) throw new Exception($"Could not find an expression node to reduce! {pc.Expression}");

        var functionToCall = actionable[0]; // this is the function name
        var paramValues    = ParametersOf(functionToCall.Parent);

        Console.WriteLine($"To process: '{functionToCall.Source.Value}' / {functionToCall.Source.Tag}");
        return DispatchFunctionCall(functionToCall, paramValues);
    }

    /// <summary>
    /// Call a function in an expression tree
    /// </summary>
    private bool DispatchFunctionCall(TreeNode<Value> function, IEnumerable<TreeNode<Value>> paramExpressions)
    {
        var name          = function.Source.Value;
        var parameterVals = paramExpressions.Select(c => c.UserData).ToList();

        if (HandleBuiltInFunction(name, parameterVals))
        {
            var pc = _returnStack.Peek();
            if (pc.ReturnValue is not null)
            {
                function.UserData = pc.ReturnValue;
                pc.ReturnValue = null;
            }
            else
            {
                AdvanceProgramPointer(); //???
            }

            /*
            // TODO: handle this in a return structure.
            var pc = _returnStack.Peek();
            if (pc.Location?.Tag == LanguageDefinition.Assignment)
            {
                Console.WriteLine($"Need to assign '{pc.ReturnValue}' to {function.Parent}?");
                return false;
            }

            */
            return true;
        }

        var ok = _functionDefs.TryGetValue(name, out var target);
        if (!ok) throw new Exception($"Function '{name}' is not defined");

        var location = target?.Children.FirstOrDefault();
        if (location is null) throw new Exception($"Function '{name}' is invalid");

        var parameterSpec = target!.Parent!.ChildrenByTag(LanguageDefinition.Parameter).Select(p => p.Value).ToList();

        if (parameterVals.Count != parameterSpec.Count) throw new Exception($"Call to '{name}' has {parameterVals.Count} parameters, but the function defines {parameterSpec.Count}.");

        _returnStack.Push(new ProgramPtr
        {
            Location = location,
            Expression = null,
            ReturnValue = function.UserData
        });

        var newScope = new VarScope( /*currentScope*/null); // maybe function calls should have no parent link?
        _scopeStack.Push(newScope);

        // Pass in parameters into new scope
        for (var index = 0; index < parameterSpec.Count; index++)
        {
            var paramValue = parameterVals[index];
            var paramName  = parameterSpec[index];
            newScope.Set(paramName, paramValue ?? new Value());
        }


        Console.WriteLine($"Calling '{name}' at {target}");

        return true;
    }

    /// <summary>
    /// Check if all a function's parameters have a value (or if there are no parameters)
    /// </summary>
    private static bool IsFunctionReadyToCall(TreeNode<Value> node)
    {
        // Note: 'All' should return true if the Children are empty
        return node.UserData is null // not already resolved
            && node.Source.Tag == LanguageDefinition.FunctionName // is a function
            && ParametersOf(node.Parent).All(n => n.Source.Tag != LanguageDefinition.Parameter || n.UserData is not null) == true; // has no parameters, or all are resolved
    }

    /// <summary>
    /// Return the call parameters for a function call in an expression
    /// </summary>
    private static IEnumerable<TreeNode<Value>> ParametersOf(TreeNode<Value>? parent)
    {
        if (parent is null) return Array.Empty<TreeNode<Value>>();
        if (parent.Children.Count < 2) return Array.Empty<TreeNode<Value>>();

        if (parent.Children[1].UserData is not null) return [parent.Children[1]]; // bare expression
        return parent.Children[1].Children; // set of values
    }

    /// <summary>
    /// Try to progress an assignment statement. Might make multiple function calls if required.
    /// </summary>
    private bool AssignInScope()
    {
        var pc = _returnStack.Peek();
        if (pc.Location is null) return false;

        var source = pc.Location.FirstByTag(LanguageDefinition.Expression);
        var target = pc.Location.FirstByTag(LanguageDefinition.Variable);
        if (source?.AnyMatch is null) throw new Exception($"Invalid assignment at {pc.Location}");
        if (target is null) throw new Exception($"Invalid assignment at {pc.Location}");
        // 1. Build the expression TreeNode, attach it to our program pointer
        // 2. Reduce the expression tree, switching to a new program pointer as required.
        // 3. When tree has a definite value, do the assignment
        // 4. If we have no value, and no reductions, throw exception.

        if (HandleExpression(pc, source))
        {
            // Got a final value
            if (pc.ReturnValue is null) throw new Exception("Invalid return value");
            SetScopeValue(name: target.Value, value: pc.ReturnValue);
            AdvanceProgramPointer();
        }

        // May need to handle the expression further
        //Console.WriteLine($"Need to call '{source.Value}' to assign result to '{target.Value}'");

        return true;
    }

    /// <summary>
    /// Try to reduce an expression. Returns true if a final value is resolved
    /// </summary>
    private bool HandleExpression(ProgramPtr pc, ScopeNode<None> source)
    {
        // Read the expression
        TreeNode<Value> exprTree;
        if (pc.Expression is not null)
        {
            exprTree = pc.Expression;
        }
        else
        {
            var tree = TreeNode<Value>.FromParserMatch(source.AnyMatch!, prune: true);
            exprTree = TreeNode<Value>.TransformTree(tree, ReduceExpressionWithMath) ?? throw new Exception($"Invalid expression: '{source.Value}'");
            Console.WriteLine("===[ expression ]=======");
            PrintRecursive(exprTree, 0);
            Console.WriteLine("========================");
        }

        if (exprTree?.UserData is not null && exprTree.UserData is not null)
        {
            pc.ReturnValue = exprTree.UserData;
            pc.Expression = null;
            // Have we reduced to a single value?
            return true;
        }


        // Otherwise it contains a function call. We will save the expression and handle it next time
        pc.Expression = exprTree;
        return false;
    }

    /// <summary>
    /// Current statement is complete. Move forward
    /// </summary>
    private void AdvanceProgramPointer()
    {
        var pc = _returnStack.Peek();

        // Reset values we track for a single program position
        pc.Expression = null;
        pc.ReturnValue = null;

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
        _scopeStack.Pop();
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

    /// <summary>
    /// Try to reduce an expression tree by applying operators and resolving variables
    /// </summary>
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

        // Is it a literal number?
        if (double.TryParse(node.Source.Value, out var dbl))
        {
            node.UserData = new Value { NumericValue = dbl, Kind = ValueKind.Numeric };
            node.Children.Clear();
            return node;
        }

        // Is it a literal string?
        if (node.Source.Tag == LanguageDefinition.QuotedString)
        {
            var value = UnpackQuotedString(node);
            node.UserData = value;
            return node;
        }

        // Is it a variable reference?
        if (node.Source.Tag == LanguageDefinition.Variable)
        {
            // Try and replace this node with a variable value
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

        Console.WriteLine($"'{node.Source.Value}' -> '{dst}'");
        var value = new Value(dst.ToString());
        node.UserData = value;
        return value;
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


    #region Diagnostic stuff

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

    #endregion Diagnostic stuff
}
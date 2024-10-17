using Gool.Results;

namespace TestLanguageImplementation.Interpreted;

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
    /// Expression that is being resolved, if any
    /// </summary>
    public TreeNode<Value>? ReturnPath;

    /// <summary>
    /// Container for final value of the expression or function call
    /// </summary>
    public Value? ReturnValue;
}
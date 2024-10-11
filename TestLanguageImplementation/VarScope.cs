namespace TestLanguageImplementation;

/// <summary>
/// Names and values for a lexical scope
/// </summary>
public class VarScope
{
    private readonly Dictionary<string, Value> _valueStack = new();

    public Value Get(string name)
    {
        if (string.IsNullOrWhiteSpace(name)) throw new Exception("invalid name");
        if (_valueStack.TryGetValue(name, out var value)) return value;
        return new Value { Kind = ValueKind.Invalid };
    }

    public void Set(string name, Value value)
    {
        if (string.IsNullOrWhiteSpace(name)) throw new Exception("invalid name");
        _valueStack[name] = value;
    }
}
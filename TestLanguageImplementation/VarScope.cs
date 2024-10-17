namespace TestLanguageImplementation;

/// <summary>
/// Names and values for a lexical scope
/// </summary>
public class VarScope
{
    private readonly Dictionary<string, Value> _values = new();

    /// <summary>
    /// Next scope up, if there is one
    /// </summary>
    public VarScope? Parent { get; }

    /// <summary>
    /// Create a new scope, linked to a parent
    /// </summary>
    public VarScope(VarScope? parent)
    {
        Parent = parent;
    }

    public Value Get(string name)
    {
        if (string.IsNullOrWhiteSpace(name)) throw new Exception("invalid name");
        if (_values.TryGetValue(name, out var value)) return value;
        return new Value { Kind = ValueKind.Invalid };
    }

    public void Set(string name, Value value)
    {
        if (string.IsNullOrWhiteSpace(name)) throw new Exception("invalid name");

        var scope = this;
        while (scope is not null)
        {
            if (scope._values.ContainsKey(name))
            {
                Console.WriteLine($"Updated '{name}' to {value}");
                _values[name] = value;
                return;
            }

            scope = scope.Parent;
        }

        Console.WriteLine($"Wrote {value} to '{name}'");
        _values.Add(name, value);
    }

}
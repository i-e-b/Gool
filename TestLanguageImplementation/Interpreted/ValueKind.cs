namespace TestLanguageImplementation.Interpreted;

/// <summary>
/// Type of a value
/// </summary>
public enum ValueKind
{
    /// <summary> Not a valid value </summary>
    Invalid = 0,

    /// <summary> A floating-point number </summary>
    Numeric = 1,

    /// <summary> A string </summary>
    String  = 2,

    /// <summary> True or False </summary>
    Boolean = 3,

    /// <summary> Location in the program (used for break/continue) </summary>
    Location = 4
}
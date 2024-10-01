using System;
using Gool.Parsers.Composite;
using Gool.Parsers.Terminals;

namespace Gool;

/// <summary>
/// Extension methods for BNF
/// </summary>
public static class BnfExtensions
{
    /// <summary>
    /// Match a literal string in a case insensitive way
    /// </summary>
    public static BNF CaseInsensitive(this string pattern)
    {
        return new BNF(new LiteralString(pattern, StringComparison.OrdinalIgnoreCase));
    }

    /// <summary>
    /// Repeat the pattern a specific number of times
    /// </summary>
    public static BNF Repeat(this string pattern, int i)
    {
        return new BNF(new Repetition((BNF)pattern, (uint)i, (uint)i));
    }

    /// <summary>
    /// Repeat the pattern a range of times
    /// </summary>
    public static BNF Repeat(this string pattern, int min, int max)
    {
        return new BNF(new Repetition((BNF)pattern, (uint)min, (uint)max));
    }

    /// <summary>
    /// Repeat the pattern a specific number of times
    /// </summary>
    public static BNF Repeat(this char pattern, int i)
    {
        return new BNF(new Repetition((BNF)pattern, (uint)i, (uint)i));
    }

    /// <summary>
    /// Repeat the pattern a range of times
    /// </summary>
    public static BNF Repeat(this char pattern, int min, int max)
    {
        return new BNF(new Repetition((BNF)pattern, (uint)min, (uint)max));
    }
}
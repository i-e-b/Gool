using System.Collections.Generic;

namespace Gool.Results;

/// <summary>
/// Extension methods for working with results
/// </summary>
public static class ResultExtensions
{
    /// <summary>
    /// Find the first result with the given tag that can be parsed as an integer.
    /// </summary>
    /// <param name="results">Results to search</param>
    /// <param name="tag">Tag to find</param>
    /// <param name="value">Resulting value. This is zero if no match is found</param>
    /// <returns><c>true</c> if a match is found, <c>false</c> if not match</returns>
    public static bool FindIntByTag(this IEnumerable<ParserMatch>? results, string tag, out int value)
    {
        value = 0;
        if (results is null) return false;

        foreach (var result in results)
        {
            foreach (var child in result.ChildrenWithTag(tag))
            {
                if (int.TryParse(child.Value, out value)) return true;
            }
        }

        return false;
    }


    /// <summary>
    /// Find the first result with the given tag that can be parsed as an integer.
    /// </summary>
    /// <param name="result">Result to search</param>
    /// <param name="tag">Tag to find</param>
    /// <param name="value">Resulting value. This is zero if no match is found</param>
    /// <returns><c>true</c> if a match is found, <c>false</c> if not match</returns>
    public static bool FindIntByTag(this ParserMatch? result, string tag, out int value)
    {
        value = 0;
        if (result is null) return false;

        foreach (var child in result.ChildrenWithTag(tag))
        {
            if (int.TryParse(child.Value, out value)) return true;
        }

        return false;
    }
}